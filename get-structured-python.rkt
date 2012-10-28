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
     (PySeq (map get-structured-python expr-list))]
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
    #|
    [(hash-table ('type "FunctionDef")
                 ('name name) ;identifier
                 ('args args) ;arguments
                 ('body body) ;stmt*
                 ('decorator_list dec_list) ;expr* ;;ignoring
                 ('returns returns)) ;expr? ;;ignoring
     (Py
    |#
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
    ;; raise
    [(hash-table ('type "Raise")
                 ('exc exc)
                 ('cause cause))
     (PyRaise (get-structured-python exc))] ;; (get-structured-python cause))]
     
                 
    ;;THE ONES THAT RETURN PRIMITIVES (symbols, numbers, strings, etc):
    
    [(hash-table ('type "Or"))
     'or]
    [(hash-table ('type "And"))
     'and]
    [(hash-table ('type "Eq"))
     'eq]
    [(hash-table ('type "NotEq"))
     'notEq]
    [(hash-table ('type "Lt"))
     'lt]
    [(hash-table ('type "LtE"))
     'lte]
    [(hash-table ('type "Gt"))
     'gt]
    [(hash-table ('type "GtE"))
     'gte]
    [(hash-table ('type "Is"))
     'is]
    [(hash-table ('type "IsNot"))
     'isNot]
    
    ;; Unary (kechpaja)
    [(hash-table ('type "Not"))
     'not]
    
    
    
                 
    [_ (begin (display "PARSING: Haven't handled a case yet: \n")
              (pretty-write pyjson)
              (error 'parse "error is described above."))]))

