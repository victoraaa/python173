#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt")


(define (desugar expr)
  (type-case PyExpr expr
;    #|
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
    [PyLambda (args body) (CFunc args (desugar body))]
    [PyRaise (exc) (CError (desugar exc))]
    
;|#
    [else (error 'desugar (string-append "Haven't desugared a case yet:\n"
                                       (to-string expr)))]))

;(test (desugar (PyBoolop 'or (list (PyNum 0) (PyNum 1))))
;      (CBoolop 'or (CNum 0) (CNum 1)))
