#lang plai

(require "python-syntax.rkt")
(require racket/match
         racket/list)

#|

Python parses as a JSON structure that we export from Python's ast
module.  You should use this file to turn it into a plai-typed data
structure that you define in python-syntax.rkt

|#

(define (get-structured-python pyjson)
  (match pyjson
    [(hash-table ('type "Module") ('body expr-list))
     (PyModule (PySeq (map get-structured-python expr-list)))]
    [(hash-table ('type "Expr") ('value expr))
     (get-structured-python expr)]
    [(hash-table ('type "Call")
                 ('keywords keywords) ;; ignoring keywords for now
                 ('kwargs kwargs)     ;; ignoring kwargs for now
                 ('starargs starargs) ;; ignoring starargs for now
                 ('args args-list)
                 ('func func-expr))
     (PyApp (get-structured-python func-expr)
            (map get-structured-python args-list))]
    
    ;; Catching None up here, before we hit the identifer case
    [(hash-table ('type "Name")
                 ('ctx _)
                 ('id "None"))
     (PyNone)]
    
    ;; Should these cases be in lib? This is something to ask at design check...   
    ;; Alternatively, they could be handled in desugaring. This is a decision we need to make
    ;; before we can proceed. I vote for desugaring. 
    
    [(hash-table ('type "Name")
                 ('ctx _)        ;; ignoring ctx for now
                 ('id id))
     (PyId (string->symbol id))]
    [(hash-table ('type "Num")
                 ('n n))
     (PyNum n)]
    ;;MADE BY ME:
    [(hash-table ('type "If")
                 ('body body) ;stmt-list
                 ('test test) ;expr
                 ('orelse orelse)) ;stmt-list
     (PyIf (get-structured-python test )
           (map get-structured-python body)
           (map get-structured-python orelse))]
    [(hash-table ('type "BoolOp")
                 ('op op) ;boolop
                 ('values values)) ;expr-list
     (PyBoolop (get-structured-python op) (map get-structured-python values))]
    [(hash-table ('type "UnaryOp")
                 ('op op)
                 ('operand operand))
     (PyUnaryOp (get-structured-python op) (get-structured-python operand))]
    [(hash-table ('type "Compare")
                 ('left left) ;expr
                 ('ops ops) ;cmpop*
                 ('comparators comparators)) ;expr*
     (PyCompare (get-structured-python left)
                (map get-structured-python ops)
                (map get-structured-python comparators))]
    [(hash-table ('type "Str")
                 ('s s))
     (PyStr s)]
    [(hash-table ('type "Pass"))
     (PyPass)]
    
    
    
    [(hash-table ('type "Lambda")
                 ('args args) ;arguments
                 ('body body)) ;expr
     (PyLambda (get-structured-python args)
               (get-structured-python body))]
    [(hash-table ('type "arguments")
                 ('args args) ;arg*
                 ('vararg vararg)
                 ('varargannotation varargannotation)
                 ('kwonlyargs kwonlyargs)
                 ('kwarg kwarg)
                 ('kwargannotation kwargannotation)
                 ('defaults defaults)
                 ('kw_defaults kw_defaults))
     (map get-structured-python args)]
    [(hash-table ('type "arg")
                 ('arg arg)
                 ('annotation annotation))
     (string->symbol arg)]
    [(hash-table ('type "Assign")
                 ('targets targets)
                 ('value value))
     (PyAssign (map get-structured-python targets)
               (get-structured-python value))]
    
    [(hash-table ('type "AugAssign")
                 ('target target)
                 ('op op)
                 ('value value))
     (PyAugAssign (get-structured-python target)
                  (get-structured-python op)
                  (get-structured-python value))]
    ;; raise
    [(hash-table ('type "Raise")
                 ('exc exc)
                 ('cause cause))
     (PyRaise (get-structured-python exc))] ;; (get-structured-python cause))]
    [(hash-table ('type "BinOp")
                 ('left left) ;expr
                 ('op op) ;operator
                 ('right right)) ;expr
     (PyBinOp (get-structured-python op)
              (get-structured-python left)
              (get-structured-python right))]
    
    
    ;; def
    [(hash-table ('type "FunctionDef")
                 ('name name)
                 ('args args)
                 ('body body)
                 ('decorator_list decorator-list)
                 ('returns returns))
     (PyDef (string->symbol name) 
            (get-structured-python args)
            (PySeq (map get-structured-python body)))]
    
    ;; return case
    [(hash-table ('type "Return")
                 ('value value))
     (PyReturn (get-structured-python value))]
    
    
    ;; global variable
    [(hash-table ('type "Global")
                 ('names names))
     (PyGlobal (map (lambda (name) (string->symbol name)) names))]
    
    ;; nonlocal variable
    [(hash-table ('type "Nonlocal")
                 ('names names))
     (PyNonlocal (map (lambda (name) (string->symbol name)) names))]
    
    ;; lists
    [(hash-table ('type "List")
                 ('elts elts)
                 ('ctx ctx))
     (PyList (map get-structured-python elts))]
    
    ;; Dicts
    [(hash-table ('type "Dict")
                 ('keys keys)
                 ('values values))
     (PyDict (map get-structured-python keys)
             (map get-structured-python values))]
                 
    ;;THE ONES THAT RETURN PRIMITIVES (symbols, numbers, strings, etc):
    
    ;; arithmetic
    [(hash-table ('type "Add"))
     'python-add]
    [(hash-table ('type "Sub"))
     'python-sub]
    [(hash-table ('type "Mult"))
     'python-mult]
    [(hash-table ('type "Div"))
     'python-div]
 ;   [(hash-table ('type "Pow")) ;; this will need to be recursive. Wait to implement...
 ;    'python-pow]
    
    [(hash-table ('type "Or"))
     'or]
    [(hash-table ('type "And"))
     'and]
    [(hash-table ('type "Eq"))
     'python-eq]
    [(hash-table ('type "NotEq"))
     'python-notEq]
    [(hash-table ('type "Lt"))
     'python-lt]
    [(hash-table ('type "LtE"))
     'python-lte]
    [(hash-table ('type "Gt"))
     'python-gt]
    [(hash-table ('type "GtE"))
     'python-gte]
    [(hash-table ('type "Is"))
     'python-is]
    [(hash-table ('type "IsNot"))
     'python-isNot]
    [(hash-table ('type "In"))
     'python-in]
    [(hash-table ('type "NotIn"))
     'python-notIn]

    
    ;; Unary
    [(hash-table ('type "Not"))
     'python-not]
    [(hash-table ('type "USub"))
     'python-negate]
    [(hash-table ('type "UAdd"))
     'python-uadd]
    [(hash-table ('type "Invert"))
     'python-invert]
    
    
    
                 
    [_ (begin (display "PARSING: Haven't handled a case yet: \n")
              (pretty-write pyjson)
              (error 'parse "error is described above."))]))

