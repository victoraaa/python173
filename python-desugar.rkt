#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt")

;; cascade-lets will build up the nested lets, and use body as the
;; eventual body, preserving order of evaluation of the expressions
(define (cascade-lets (ids : (listof symbol))
                      (sts : (listof ScopeType))
                      (exprs : (listof CExp))
                      (body : CExp)) : CExp
  (cond [(empty? ids) body]
        [(cons? ids)
         (CLet (first ids) (first sts) (first exprs) (cascade-lets (rest ids) (rest sts) (rest exprs) body))]))


(define (get-vars [expr : PyExpr]) : (listof (ScopeType * symbol))
  (type-case PyExpr expr
    [PyNonlocal (id) (list (values (NonLocal) id))]
    [PyGlobal (id) (list (values (Global) id))]
    [PySeq (es) (foldl (lambda (a b) (append b a))
                       (list)
                       (map (lambda (e) (get-vars e)) es))]
    [PyNum (n) (list)]
    [PyApp (f args) (append (get-vars f)
                            (foldl (lambda (a b) (append b a))
                                   (list)
                                   (map (lambda (e) (get-vars e)) args)))]
    [PyId (x) (list)]
    [PyStr (s) (list)]
    [PyIf (test then orelse)
          (append
           (get-vars test)
           (append
            (foldl (lambda (a b) (append b a))
                   (list)
                   (map (lambda (e) (get-vars e)) then))
            (foldl (lambda (a b) (append b a))
                   (list)
                   (map (lambda (e) (get-vars e)) orelse))))]
    [PyBoolop (op exprs)
              (foldl (lambda (a b) (append b a))
                                   (list)
                                   (map (lambda (e) (get-vars e)) exprs))]
    [PyCompare (left ops comparators)
               (append
                (get-vars left)
                (foldl (lambda (a b) (append b a))
                                   (list)
                                   (map (lambda (e) (get-vars e)) comparators)))]
    [PyPass () (list)] ;; won't typecheck without this
    [PyNone () (list)]
    [PyLambda (args body) (list)]
    [PyRaise (exc) (get-vars exc)]
    [Py-NotExist () (list)]
    [PyUnaryOp (op arg) (get-vars arg)]
    [PySet (lhs value) ;;PySet case may need to change when it starts to get things other than CIds
           (append
               (get-vars value)
               (type-case PyExpr lhs
                 [PyId (id) (list (values (Local) id))]
                 [else (error 'get-vars-PySet "PySet should not be getting non-ids yet")]))]
    [PyAssign (targets value)
              (append
               (get-vars value)
               (foldl (lambda (a b) (append b a))
                      (list)
                      (map (lambda (e) (type-case PyExpr e
                                         [PyId (id) (list (values (Local) id))]
                                         [else (error 'get-vars-PyAssign "PyAssign should not be getting non-ids yet")])) 
                           targets)))]
    [PyModule (exprs)
              (get-vars exprs)]
    ))



(define (desugar expr)
  (type-case PyExpr expr
 ;   #|
    [PySeq (es) (foldr (lambda (e1 e2) (CSeq e2 (desugar e1))) (desugar (first es)) (rest es))]
    [PyNum (n) (CNum n)]
    [PyApp (f args) (CApp (desugar f) (map desugar args))]
    [PyId (x) (CId x)]
    ;;Under this, Non-TA code
    [PyStr (s) (CStr s)]
    [PyIf (test then orelse)
          (CIf (desugar test) 
               (if (> (length then) 1) (desugar (PySeq then)) (desugar (first then)))
               (if (> (length orelse) 0) 
                   (if (> (length orelse) 1)
                       (desugar (PySeq orelse)) 
                       (desugar (first orelse)))
                   (CNone)))]
    [PyBoolop (op exprs)
              (case op
                ['or (foldl (lambda (expr result) (CPrim2 'or result (desugar expr))) (desugar (first exprs)) (rest exprs))]
                ['and (foldl (lambda (expr result) (CPrim2 'and result (desugar expr))) (desugar (first exprs)) (rest exprs))])]
    [PyUnaryOp (op arg)
               (CPrim1 op (desugar arg))]
    [PyCompare (left ops comparators)
               (if (equal? 0 (length comparators))
                   (CTrue)
                   (CLet 'left-comp (Local) (desugar left)
                     (CLet 'right-comp (Local) (desugar (first comparators))
                       (CIf (CPrim2 (first ops)
                                    (CId 'left-comp)
                                    (CId 'right-comp))
                            (desugar (PyCompare (PyId 'right-comp)
                                                (rest ops)
                                                (rest comparators)))
                            (CFalse)))))]
    
    [PyPass () (CNone)]
    [PyNone () (CNone)]
    [PyLambda (args body) (CFunc args (desugar body))]
    #|(FuncC args 
                              (let ([list-vars (get-vars body)])
                                (cascade-lets list-vars
                                              (list-undefinedC (length list-vars))
                                              (desugar body))))]
|#
    [PyRaise (exc) (CError (desugar exc))]
    [PyAssign (targets value) 
              (CLet 'assign-value (Local) (desugar value)
                    (desugar (PySeq (map (lambda (e) (PySet e (PyId 'assign-value))) targets))))]
    [PySet (lhs value) (CSet (desugar lhs) (desugar value))]
    [PyModule (exprs) 
              (let ([global-vars (get-vars exprs)]) ;GET ALL OF THE ASSIGNMENTS IN THE GLOBAL SCOPE
                (begin ;(checkGlobalScopes global-vars)  ;CHECKS IF WE DONT HAVE AN ERROR FROM USING global OR nonlocals IN THE GLOBAL SCOPE
                        ;WE NEED TO PUT THEM IN THE GLOBAL ENVIRONMENT AS WELL
                       (cascade-lets (get-ids global-vars) ;PUT THEM IN THE ENVIRONMENT AS LOCALS
                                     (make-item-list (Local) (length global-vars) (list)) 
                                     (make-item-list (CUnbound) (length global-vars) (list)) 
                                     (desugar exprs))))] ;EXECUTE THE exprs (desugar exprs)
                

    
;|#
    [else (error 'desugar (string-append "Haven't desugared a case yet:\n"
                                       (to-string expr)))]))

(define (make-item-list [item : 'a]
                        [size : number]
                        [newList : (listof 'a)]) : (listof 'a)
  (cond 
    [(>= (length newList) size) newList]
    [else (make-item-list item size (append (list item) newList))]))

(define (get-ids [vars-list : (listof (ScopeType * symbol))]) : (listof symbol)
  (foldl (lambda (a b) (append b a))
                       (list)
                       (map (lambda (e) (local ([define-values (st id) e])
                                                      (list id)))
                              vars-list)))

(define (get-sts [vars-list : (listof (ScopeType * symbol))]) : (listof ScopeType)
  (foldl (lambda (a b) (append b a))
                       (list)
                       (map (lambda (e) (local ([define-values (st id) e])
                                                      (list st)))
                              vars-list)))

;(test (desugar (PyBoolop 'or (list (PyNum 0) (PyNum 1))))
;      (CBoolop 'or (CNum 0) (CNum 1)))
