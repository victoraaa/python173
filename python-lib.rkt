#lang plai-typed

(require "python-core-syntax.rkt")

#|

Here is a suggestion for how to implement shared runtime functionality -
write it as core expression forms and use python-lib to wrap your
desugared expressions in an environment that will contain useful
bindings.  For example, this sample library binds `print` to a function
that calls the primitive `print`.

|#

(define-type-alias Lib (CExp -> CExp))

(define print-lambda
  (CFunc (list 'to-print)
    (CPrim1 'print (CId 'to-print)) (list) (list) 'no-vararg))

(define assert-equal-lambda
  (CFunc (list 'e-1 'e-2)
    (CIf (CApp (CId 'python-eq) 
               (list (CId 'e-1) (CId 'e-2)) 
               (list) 
               (CHash (hash (list)) (cType "list" (CNone)))
               )
         (CPass) 
         (CError (CStr "Assert failed: values are not Equal"))
         ) 
    (list)
    (list)
    'no-vararg))

(define assert-notEqual-lambda
  (CFunc (list 'e-1 'e-2)
    (CIf (CPrim2 'eq (CId 'e-1) (CId 'e-2))  
         (CError (CStr "Assert failed: values are Equal"))
         (CPass)
         )
    (list)
    (list)
    'no-vararg))

(define assert-true-lambda
  (CFunc (list 'check-true)
    (CIf (CId 'check-true) (CPass) (CError (CStr "Assert failed: value is False")))
    (list)
    (list)
    'no-vararg))

(define assert-false-lambda
  (CFunc (list 'check-false)
    (CIf (CId 'check-false) (CError (CStr "Assert failed: value is True")) (CPass) )
    (list)
    (list)
    'no-vararg))

(define assert-is-lambda
  (CFunc (list 'e-1 'e-2)
    (CIf (CPrim2 'is (CId 'e-1) (CId 'e-2)) 
         (CPass) 
         (CError (CStr "Assert failed: first argument is not second argument"))
         )
    (list)
    (list)
    'no-vararg))

(define assert-isNot-lambda
  (CFunc (list 'e-1 'e-2)
    (CIf (CPrim2 'is (CId 'e-1) (CId 'e-2)) 
         (CError (CStr "Assert failed: first argument is second argument"))
         (CPass) 
         )
    (list)
    (list)
    'no-vararg))

(define assert-in-lambda
  (CFunc (list 'e-1 'e-2)
    (CIf (CPrim2 'in (CId 'e-1) (CId 'e-2)) 
         (CPass) 
         (CError (CStr "Assert failed: element not found"))
         )
    (list)
    (list)
    'no-vararg))

(define assert-notIn-lambda
  (CFunc (list 'e-1 'e-2)
    (CIf (CPrim2 'is (CId 'e-1) (CId 'e-2)) 
         (CError (CStr "Assert failed: element found"))
         (CPass)
         )
    (list)
    (list)
    'no-vararg))


(define assert-raises-lambda
  (CFunc (list 'e-1 'e-2)
         (CTryExcept (CApp (CId 'e-2) (list) (list) (CId 'vargs)) 
                     (list (CExcHandler 'e (CId 'e-1) (CTrue)))
                     (CFalse))
         (list)
         (list)
         'vargs))

;; TODO can this be re-written with instanceof?

;(define assert-raises-lambda
;  (CFunc (list 'e-1 'e-2)
; (CTryExcept (CApp (CId 'e-2) (CPrim1 'python-star (CId 'args)) (list)) (CExcHandler 'e () (CTrue)) (CFalse))
                                       ;; Or however it ends up working...
; (list)
; (list)
; 'args))



;; math
(define python-add
  (CFunc (list 'e-1 'e-2)
         (CIf (CPrim2 'and
                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "string"))
                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "string")))
              (CPrim2 'string+ (CId 'e-1) (CId 'e-2))
              (CIf (CPrim2 'or
                           (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                           (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "float")))
                   (CIf (CPrim2 'or
                                (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "int"))
                                (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "float")))
                        (CPrim2 'num+ (CId 'e-1) (CId 'e-2))
                        (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "bool"))
                             (CPrim2 'num+ (CId 'e-1) (CPrim1 'to-int (CId 'e-2)))
                             (CError (CStr "+: Cannot do math on this type!"))))
                   (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "bool"))
                        (CIf (CPrim2 'or
                                     (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "int"))
                                     (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "float")))
                             (CPrim2 'num+ (CPrim1 'to-int (CId 'e-1)) (CId 'e-2))
                             (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "bool"))
                                  (CPrim2 'num+ (CPrim1 'to-int (CId 'e-1)) (CPrim1 'to-int (CId 'e-2)))
                                  (CError (CStr "+: Cannot do math on this type!"))))
                        (CIf (CPrim2 'and 
                                     (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "list"))
                                     (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "list")))
                             (CPrim2 'list+ (CId 'e-1) (CId 'e-2))
                             (CIf (CPrim2 'and 
                                          (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "tuple"))
                                          (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "tuple")))
                                  (CPrim2 'tuple+ (CId 'e-1) (CId 'e-2))
                                  (CError (CStr "+: Cannot do math on this type... Sorry!")))))))
         (list)
         (list)
         'no-vararg))


;; handles addition
;(define python-add
;  (CFunc (list 'e-1 'e-2)
;         (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CPrim1 'tagof (CId 'e-2)))
;              (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
;                   (CPrim2 'num+ (CId 'e-1) (CId 'e-2))
;                   (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "float"))
;                        (CPrim2 'num+ (CId 'e-1) (CId 'e-2))
;                        (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "string"))
;                             (CPrim2 'string+ (CId 'e-1) (CId 'e-2))
;                             (CError (CStr "+: Not supported for this type.")))))
;              (CError (CStr "+: Types do not match.")))))

(define python-sub
  (CFunc (list 'e-1 'e-2)
         (CIf (CPrim2 'or
                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "float")))
              (CIf (CPrim2 'or
                           (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "int"))
                           (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "float")))
                   (CPrim2 'num- (CId 'e-1) (CId 'e-2))
                   (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "bool"))
                        (CPrim2 'num- (CId 'e-1) (CPrim1 'to-int (CId 'e-2)))
                        (CError (CStr "-: Cannot do math on this type!"))))
              (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "bool"))
                   (CIf (CPrim2 'or
                                (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "int"))
                                (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "float")))
                        (CPrim2 'num- (CPrim1 'to-int (CId 'e-1)) (CId 'e-2))
                        (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "bool"))
                             (CPrim2 'num- (CPrim1 'to-int (CId 'e-1)) (CPrim1 'to-int (CId 'e-2)))
                             (CError (CStr "-: Cannot do math on this type!"))))
                   (CError (CStr "-: Cannot do math on this type... Sorry!"))))
         (list)
         (list)
         'no-vararg))

(define python-mult ;; eventaully, this has to work for strings and integers too...
  (CFunc (list 'e-1 'e-2)
         (CIf (CPrim2 'or
                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "float")))
              (CIf (CPrim2 'or
                           (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "int"))
                           (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "float")))
                   (CPrim2 'num* (CId 'e-1) (CId 'e-2))
                   (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "bool"))
                        (CPrim2 'num* (CId 'e-1) (CPrim1 'to-int (CId 'e-2)))
                        (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "string"))
                             (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                                  (CPrim2 'duplicate (CId 'e-2) (CId 'e-1))
                                  (CError (CStr "*: Cannot multiply string by float type!")))
                             (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "tuple"))
                                  (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                                       (CPrim2 'duple (CId 'e-2) (CId 'e-1))
                                       (CError (CStr "*: Cannot multiply string by float type!")))
                                  (CError (CStr "*: Cannot do math on this type!"))))))
              (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "bool"))
                   (CIf (CPrim2 'or
                                (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "int"))
                                (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "float")))
                        (CPrim2 'num* (CPrim1 'to-int (CId 'e-1)) (CId 'e-2))
                        (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "bool"))
                             (CPrim2 'num* (CPrim1 'to-int (CId 'e-1)) (CPrim1 'to-int (CId 'e-2)))
                             (CError (CStr "*: Cannot do math on this type!"))))
                   (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "string"))
                       (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "int"))
                            (CPrim2 'duplicate (CId 'e-1) (CId 'e-2))
                            (CError (CStr "*: Cannot do math on these types... Sorry!")))
                       (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "tuple"))
                            (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "int"))
                                 (CPrim2 'duple (CId 'e-1) (CId 'e-2))
                                 (CError (CStr "*: Cannot do math on these types... Sorry!")))
                            (CError (CStr "*: Cannot do math on this type... Sorry!"))))))
         (list)
         (list)
         'no-vararg))

;; Need to convert this function as well. Divison must handle booleans.
(define python-div
  (CFunc (list 'e-1 'e-2)
         (CIf (CPrim2 'and 
                      (CPrim2 'or
                              (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                              (CPrim2 'or
                                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "float"))
                                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "bool"))))
                      (CPrim2 'or
                              (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "int"))
                              (CPrim2 'or
                                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "float"))
                                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "bool")))))
              (CIf (CPrim2 'eq (CPrim1 'to-float (CId 'e-2)) (CNum 0.0))
                   (CError (CApp (CId 'ZeroDivisionError)
                                 (list)
                                 (list)
                                 (CHash (hash (list)) (cType "list" (CId 'list)))))
                   (CPrim2 'num/ (CPrim1 'to-float (CId 'e-1)) (CPrim1 'to-float (CId 'e-2))))
              (CError (CStr "/: Not supported for this type.")))
         (list)
         (list)
         'no-vararg))

;; floor div
(define python-floor-div
  (CFunc (list 'e-1 'e-2)
         (CPrim1 'to-float (CPrim1 'to-int (CApp (CId 'python-div)
                                                 (list (CId 'e-1) (CId 'e-2))
                                                 (list)
                                                 (CHash (hash (list)) (cType "list" (CId 'list))))))
         (list)
         (list)
         'no-vararg))


(define python-mod
  (CFunc (list 'e-1 'e-2)
         (CIf (CPrim2 'and
                      (CPrim2 'or 
                              (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                              (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "float")))
                      (CPrim2 'or 
                              (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "int"))
                              (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "float"))))
              (CIf (CPrim2 'eq (CPrim1 'to-float (CId 'e-2)) (CNum 0.0))
                   (CError (CApp (CId 'ZeroDivisionError)
                                 (list)
                                 (list)
                                 (CHash (hash (list)) (cType "list" (CId 'list)))))
                   (CPrim2 'num% (CId 'e-1) (CId 'e-2)))
              (CError (CStr "%: Not supported for this type.")))
         (list)
         (list)
         'no-vararg))
                                                 



;(define python-div
;  (CFunc (list 'e-1 'e-2)
;         (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CPrim1 'tagof (CId 'e-2)))
;              (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
;                   (CIf (CPrim2 'eq (CId 'e-2) (CNum 0))
;                        (CError (CStr "/: Divide by zero"))
;                        (CPrim2 'num/ (CId 'e-1) (CId 'e-2)))
;                   (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "float"))
;                        (CIf (CPrim2 'eq (CId 'e-2) (CNum 0))
;                             (CError (CStr "/: Divide by zero"))
;                             (CPrim2 'num/ (CId 'e-1) (CId 'e-2)))
;                        (CError (CStr "/: Not supported for this type."))))
;              (CError (CStr "/: Types do not match.")))
;         (list)))

(define python-lt
  (CFunc (list 'e-1 'e-2)
         (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CPrim1 'tagof (CId 'e-2)))
              (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                   (CPrim2 'num-lt (CId 'e-1) (CId 'e-2))
                   (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "float"))
                        (CPrim2 'num-lt (CId 'e-1) (CId 'e-2))
                        (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "string"))
                             (CPrim2 'string-lt (CId 'e-1) (CId 'e-2))
                             (CError (CStr "<: Not supported for this type.")))))
              (CError (CStr "<: Types do not match.")))
         (list)
         (list)
         'no-vararg))

(define python-lte
  (CFunc (list 'e-1 'e-2)
         (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CPrim1 'tagof (CId 'e-2)))
              (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                   (CPrim2 'num-lte (CId 'e-1) (CId 'e-2))
                   (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "float"))
                        (CPrim2 'num-lte (CId 'e-1) (CId 'e-2))
                        (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "string"))
                             (CPrim2 'string-lte (CId 'e-1) (CId 'e-2))
                             (CError (CStr "<=: Not supported for this type.")))))
              (CError (CStr "<=: Types do not match.")))
         (list)
         (list)
         'no-vararg))

(define python-gt
  (CFunc (list 'e-1 'e-2)
         (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CPrim1 'tagof (CId 'e-2)))
              (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                   (CPrim2 'num-gt (CId 'e-1) (CId 'e-2))
                   (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "float"))
                        (CPrim2 'num-gt (CId 'e-1) (CId 'e-2))
                        (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "string"))
                             (CPrim2 'string-gt (CId 'e-1) (CId 'e-2))
                             (CError (CStr ">: Not supported for this type.")))))
              (CError (CStr ">: Types do not match.")))
         (list)
         (list)
         'no-vararg))

(define python-gte
  (CFunc (list 'e-1 'e-2)
         (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CPrim1 'tagof (CId 'e-2)))
              (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                   (CPrim2 'num-gte (CId 'e-1) (CId 'e-2))
                   (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "float"))
                        (CPrim2 'num-gte (CId 'e-1) (CId 'e-2))
                        (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "string"))
                             (CPrim2 'string-gte (CId 'e-1) (CId 'e-2))
                             (CError (CStr ">=: Not supported for this type.")))))
              (CError (CStr ">=: Types do not match.")))
         (list)
         (list)
         'no-vararg))

(define python-uadd
  (CFunc (list 'e-1)
         (CIf (CPrim2 'or 
                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int")) 
                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "float")))
              (CId 'e-1)
              (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "bool"))
                   (CPrim1 'to-int (CId 'e-1))
                   (CError (CStr "Unary +: Not supported for this type."))))
         (list)
         (list)
         'no-vararg))

;; TODO: need invert, negate, and not cases. With typechecking. 


(define python-invert
  (CFunc (list 'e-1)
         (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
              (CPrim1 'invert (CId 'e-1))
              (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "bool"))
                   (CPrim1 'invert (CPrim1 'to-int (CId 'e-1)))
                   (CError (CStr "~: Cannot invert this type."))))
         (list)
         (list)
         'no-vararg))


(define python-eq
  (CFunc (list 'e-1 'e-2)
         (CIf (CPrim2 'and 
                      (CPrim2 'or
                              (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                              (CPrim2 'or
                                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "float"))
                                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "bool"))))
                      (CPrim2 'or
                              (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "int"))
                              (CPrim2 'or
                                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "float"))
                                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "bool")))))
              (CPrim2 'eq (CPrim1 'to-float (CId 'e-1)) (CPrim1 'to-float (CId 'e-2)))
              (CPrim2 'eq (CId 'e-1) (CId 'e-2)))
         (list)
         (list)
         'no-vararg))

(define python-notEq
  (CFunc (list 'e-1 'e-2)
         (CPrim2 'notEq (CId 'e-1) (CId 'e-2))
         (list)
         (list)
         'no-vararg))

(define python-is
  (CFunc (list 'e-1 'e-2)
         (CPrim2 'is (CId 'e-1) (CId 'e-2))
         (list)
         (list)
         'no-vararg))

(define python-isNot
  (CFunc (list 'e-1 'e-2)
         (CPrim1 'not (CPrim2 'is (CId 'e-1) (CId 'e-2)))
         (list)
         (list)
         'no-vararg))

(define python-in
  (CFunc (list 'e-1 'e-2)
         (CPrim2 'in (CId 'e-1) (CId 'e-2))
         (list)
         (list)
         'no-vararg))


(define python-notIn
  (CFunc (list 'e-1 'e-2)
         (CPrim1 'not (CPrim2 'in (CId 'e-1) (CId 'e-2)))
         (list)
         (list)
         'no-vararg))

(define print
  (CFunc (list 'e-1)
         (CPrim1 'print (CId 'e-1))
         (list)
         (list)
         'no-vararg))

(define python-not
  (CFunc (list 'e-1)
         (CPrim1 'not (CId 'e-1))
         (list)
         (list)
         'no-vararg))

(define python-negate
  (CFunc (list 'e-1)
         (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "bool"))
              (CPrim1 'negative (CPrim1 'to-int (CId 'e-1)))
              (CIf (CPrim2 'or
                           (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                           (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "float")))
                   (CPrim1 'negative (CId 'e-1)) ;; can be made much more efficient...
                   (CError (CStr "Unary -: Not supported for this type."))))
         (list)
         (list)
         'no-vararg))

(define len 
  (CFunc (list 'e-1)
         (CIf (CPrim2 'or
                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "string"))
                      (CPrim2 'or
                              (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "list"))
                              (CPrim2 'or
                                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "dict"))
                                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "tuple")))))
              (CPrim1 'length (CId 'e-1))
              (CError (CStr "len: Argument must be a string, list or dict (so far...).")))
         (list)
         (list)
         'no-vararg))

(define abs
  (CFunc (list 'e-1)
         (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "bool"))
              (CPrim1 'to-int (CId 'e-1))
              (CIf (CPrim2 'or
                           (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                           (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "float")))
                   (CIf (CPrim2 'lt (CId 'e-1) (CNum 0))
                        (CPrim1 'negative (CId 'e-1))
                        (CId 'e-1)) ;; is this the right way to do it?
                   (CError (CStr "abs: Argument must be a number or boolean"))))
         (list)
         (list)
         'no-vararg))

;; Callable
;; may need to re-write this in the future - it depends. I don't know yet. 
(define callable
  (CFunc (list 'e-1)
         (CIf (CPrim2 'or 
                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "function"))
                      (CPrim2 'or 
                              (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "class"))
                              (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "primitive-class"))))
              (CTrue)
              (CFalse))
         (list)
         (list)
         'no-vararg))


#|
(define bool ;; needs to handle arbitrary-arity input
  (CFunc (list 'e-1)
         (CPrim1 'to-bool (CId 'e-1))
         (list)
         (list (CFalse))
         'no-vararg))
|#

(define bool-primitive-class
  (CHash (hash-set (hash-set (hash (list)) 
                             (CStr "__name__") 
                             (CStr "bool")) 
                   (CStr "__convert__") 
                   (CFunc (list 'e-1)
                          (CPrim1 'to-bool (CId 'e-1))
                          (list)
                          (list (CFalse))
                          'no-vararg)) 
         (cType "primitive-class" (CNone))))

(define int-primitive-class
  (CHash (hash-set (hash-set (hash (list)) ;; TYPES!
                             (CStr "__name__") 
                             (CStr "int")) 
                   (CStr "__convert__") 
                   (CFunc (list 'e-1)
                          (CPrim1 'to-int (CId 'e-1))
                          (list)
                          (list (CNum 0))
                          'no-vararg)) 
         (cType "primitive-class" (CNone))))

(define float-primitive-class
  (CHash (hash-set (hash-set (hash (list)) ; TODO TYPE CHECKING!
                             (CStr "__name__") 
                             (CStr "float")) 
                   (CStr "__convert__") 
                   (CFunc (list 'e-1)
                          (CPrim1 'to-float (CId 'e-1))
                          (list)
                          (list (CNum 0.0))
                          'no-vararg)) 
         (cType "primitive-class" (CNone))))

(define str-primitive-class
  (CHash (hash-set (hash-set (hash (list)) 
                             (CStr "__name__") 
                             (CStr "string")) 
                   (CStr "__convert__") 
                   (CFunc (list 'e-1)
                          (CPrim1 'to-string (CId 'e-1))
                          (list)
                          (list (CStr ""))
                          'no-vararg)) 
         (cType "primitive-class" (CNone))))

(define list-primitive-class
  (CHash (hash-set (hash-set (hash (list)) 
                             (CStr "__name__") 
                             (CStr "list")) 
                   (CStr "__convert__") 
                   (CFunc (list 'e-1)
                          (CIf (CPrim2 'or
                                       (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "list"))
                                       (CPrim2 'or 
                                               (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "tuple"))
                                               (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "string"))))
                               (CPrim1 'to-list (CId 'e-1))
                               (CError (CId 'TypeError))) ;; TODO be more specific!
                          (list)
                          (list (CStr ""))
                          'no-vararg)) 
         (cType "primitive-class" (CNone))))

(define tuple-primitive-class
  (CHash (hash-set (hash-set (hash (list)) 
                             (CStr "__name__") 
                             (CStr "tuple")) 
                   (CStr "__convert__") 
                   (CFunc (list 'e-1)
                          (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "tuple")) 
                               (CId 'e-1)
                               (CPrim1 'to-tuple (CId 'e-1)))
                          (list)
                          (list (CHash (hash (list)) (cType "tuple" (CId 'tuple))))
                          'no-vararg)) 
         (cType "primitive-class" (CNone))))

(define dict-primitive-class
  (CHash (hash (list (values (CStr "__name__") (CStr "dict"))
            ;   (values (CStr "get") (CFunc (list 'self 'e-1)
            ;                                 ()
            ;                                 (list)
            ;                                 (list)
            ;                                 'no-vararg))
               ))
         (cType "primitive-class" (CNone)))) ;; If we need a __convert__ method, we'll write one later. 


(define make-range
  (CFunc (list 'e-1 'e-2 'e-3)
         (CIf (CPrim2 'and
                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                      (CPrim2 'and 
                              (CPrim2 'is (CId 'e-2) (CNone)) 
                              (CPrim2 'is (CId 'e-3) (CNone))))
              (CApp (CId 'python-make-range) 
                    (list (CNum 0) (CId 'e-1) (CNum 1)) 
                    (list)
                    (CHash (hash (list)) (cType "list" (CId 'list)))
                    )
              (CIf (CPrim2 'and
                           (CPrim2 'is (CId 'e-3) (CNone))
                           (CPrim2 'and
                                   (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                                   (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "int"))))
                   (CApp (CId 'python-make-range) 
                         (list (CId 'e-1) (CId 'e-2) (CNum 1)) 
                         (list)
                         (CHash (hash (list)) (cType "list" (CId 'list)))
                         )
                   (CIf (CPrim2 'and 
                                (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                                (CPrim2 'and
                                        (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "int"))
                                        (CPrim2 'eq (CPrim1 'tagof (CId 'e-3)) (CStr "int"))))
                        (CIf (CPrim1 'not (CPrim2 'eq (CId 'e-3) (CNum 0)))
                             (CApp (CId 'python-make-range) 
                              (list (CId 'e-1) (CId 'e-2) (CId 'e-3)) 
                              (list)
                              (CHash (hash (list)) (cType "list" (CId 'list)))
                              )
                             (CError (CApp (CId 'ValueError)
                                           (list)
                                           (list)
                                           (CHash (hash (list)) (cType "list" (CNone))))))
                        (CApp (CId 'TypeError)
                              (list)
                              (list)
                              (CHash (hash (list)) (cType "list" (CNone))))))) 
         (list)
         (list (CNone) (CNone))
         'no-vararg))

(define python-make-range
  (CLet 'python-make-range
        (Local)
        (CFunc (list) (CError (CStr "Dummy! (python-make-range)")) (list) (list) 'no-vararg)
        (CSet (CId 'python-make-range)
              (CFunc (list 'e-1 'e-2 'e-3)
                     (CIf (CPrim2 'eq (CId 'e-1) (CId 'e-2))
                          (CHash (hash (list)) (cType "list" (CId 'list)))
                          (CPrim2 'list+ 
                                  (CHash (hash-set (hash (list)) (CNum 0) (CId 'e-1)) (cType "list" (CId 'list)))
                                  (CApp (CId 'python-make-range) 
                                        (list (CPrim2 'num+ (CId 'e-1) (CId 'e-3)) (CId 'e-2) (CId 'e-3))
                                        (list)
                                        (CHash (hash (list)) (cType "list" (CId 'list)))
                                        )))
                     (list)
                     (list)
                     'no-vararg))))


#|
;; any
(define python-any
  (CFunc (list 'e-1)
         (CIf (CPrim2 'or 
                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "list"))
                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "tuple"))) ; Should theoretically work on dicts too, if we have time...
              (CPrim1 'to-bool 
                      (CApp (CId 'python-iter-help)
                            (list (CId 'e-1) (CId 'bool) (CNum 0))
                            (list)
                            (CHash (hash (list)) (Type "list" (list)))))
              (CError (CId 'TypeError))) ;; TODO more specific?
         (list)
         (list)
         'no-vararg))

;; all
(define python-all
  (CFunc (list 'e-1)
         (CIf (CPrim2 'or 
                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "list"))
                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "tuple"))) ; Should theoretically work on dicts too, if we have time...
              (CPrim1 'not
                      (CApp (CId 'python-iter-help)
                            (list (CId 'e-1) (CId 'python-not) (CNum 0))
                            (list)
                            (CHash (hash (list)) (Type "list" (list)))))
              (CError (CId 'TypeError))) ;; TODO more specific?
         (list)
         (list)
         'no-vararg))

;; filter
(define python-filter
  (CFunc (list 'e-1 'e-2)
         (CIf (CPrim2 'eq (CId 'e-1) (CNone))
              (CApp (CId 'python-iter-help)
                    (list (CId 'e-2) (CId 'bool) (CNum 0))
                    (list)
                    (CHash (hash (list)) (Type "list" (list))))
              (CIf (CApp (CId 'callable)
                         (list (CId 'e-1))
                         (list)
                         (CHash (hash (list)) (Type "list" (list))))
                   (CApp (CId 'python-iter-help)
                         (list (CId 'e-2) (CId 'e-1) (CNum 0))
                         (list)
                         (CHash (hash (list)) (Type "list" (list))))
                   (CError (CId 'TypeError))))
         (list)
         (list)
         'no-vararg))


;;helper for any and all and filter
(define python-iter-help
  (CLet 'python-iter-help
        (Local)
        (CFunc (list) (CError (CStr "Dummy! (python-iter-help)")) (list) (list) 'no-vararg)
        (CSet (CId 'python-iter-help)
              (CFunc (list 'e-list 'e-test 'e-index)
                     (CIf (CApp (CId 'python-lte)
                                (list (CId 'e-index) (CApp (CId 'len)
                                                           (list (CId 'e-list))
                                                           (list)
                                                           (CHash (hash (list)) (Type "list" (list)))))
                                (list)
                                (CHash (hash (list)) (Type "list" (list))))
                          (CIf (CApp (CId 'e-test) ;; TODO might want to check the type here. Or just in interpreter...
                                     () ;; check subscript 'e-index of list
                                     (list)
                                     (CHash (hash (list)) (Type "list" (list))))
                               (CPrim2 'list+ ;; check for subscript on next line as well...
                                       (CHash (hash-set (hash (list)) (CNum 0) (SUBSCRIPT 'e-index)) (Type "list" (list))) 
                                       (CApp (CId 'python-iter-help)
                                             (list (CId 'e-list) (CId 'e-test) (CPrim2 num+ (CId 'e-index) (CNum 1)))
                                             (list)
                                             (CHash (hash (list)) (Type "list" (list)))))
                               (CApp (CId 'python-iter-help)
                                     (list (CId 'e-list) (CId 'e-test) (CPrim2 num+ (CId 'e-index) (CNum 1)))
                                     (list)
                                     (CHash (hash (list)) (Type "list" (list)))))
                          (CHash (hash (list)) (Type "list" (list))))
                     (list)
                     (list)
                     'no-vararg))))
|#        


(define python-isinstance
  (CFunc (list 'e-1 'e-2) ;; TODO THIS HAS NO INHERITENCE! We'll want to return a list of inherited classes, and check membership...
         (CIf (CPrim2 'or
                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "class"))
                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "primitive-class")))
              (CPrim2 'or
                      (CPrim2 'isinstance (CId 'e-1) (CAttribute '__name__ (CId 'e-2)))
                      (CPrim2 'and
                              (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "bool"))
                              (CPrim2 'eq (CAttribute '__name__ (CId 'e-2)) (CStr "int"))))
              (CError (CApp (CId 'TypeError)
                            (list)
                            (list)
                            (CHash (hash (list)) (cType "list" (CNone))))))
         (list)
         (list)
         'no-vararg))
         
               
(define create-global-env
  (CFunc (list)
         (CGlobalEnv)
         (list)
         (list)
         'no-vararg))
         

(define true-val
  (CTrue))

(define python-fail
  (CFunc (list 'e-1)
         (CError (CId 'e-1))
         (list)
         (list)
         'no-vararg))



;; TODO TODO TODO update these to work like the final classs system...
(define type-error-def
  ;(CClass (hash (list)) (Type "TypeError" (VNone)))) ;; TEMPORARY TypeError definition...
  (CHash (hash (list (values (CStr "__name__") (CStr "TypeError")))) (cType "class" (CNone))))

(define index-error-def
  ;(CClass (hash (list)) (Type "IndexError" (VNone))))
  (CHash (hash (list (values (CStr "__name__") (CStr "IndexError")))) (cType "class" (CNone))))

(define zero-division-error
  ;(CClass (hash (list)) (Type "ZeroDivisonError" (VNone))))
  (CHash (hash-set (hash (list)) (CStr "__name__") (CStr "ZeroDivisionError")) (cType "class" (CNone))))

(define key-error
  ;(CClass (hash (list)) (Type "KeyError" (VNone))))
  (CHash (hash (list (values (CStr "__name__") (CStr "KeyError")))) (cType "class" (CNone))))

(define runtime-error
  ;(CClass (hash (list)) (Type "RuntimeError" (VNone))))
  (CHash (hash (list (values (CStr "__name__") (CStr "RuntimeError")))) (cType "class" (CNone))))

(define unbound-local-error
  ;(CClass (hash (list)) (Type "UnboundLocalError" (VNone))))
  (CHash (hash (list (values (CStr "__name__") (CStr "UnboundLocalError")))) (cType "class" (CNone))))

(define name-error
  ;(CClass (hash (list)) (Type "NameError" (VNone))))
  (CHash (hash (list (values (CStr "__name__") (CStr "NameError")))) (cType "class" (CNone))))

(define Exception
  ;(CClass (hash (list)) (Type "Exception" (CNone))))
  (CHash (hash (list (values (CStr "__name__") (CStr "Exception")))) (cType "class" (CNone))))



(define-type LibBinding
  [bind (left : symbol) (right : CExp)])

;; Here, we define built-in CLASSES


(define TestClass
  (CHash (hash (list (values (CStr "f") (CFunc (list)
                                               (CPrim1 'print (CStr "An Exception has been thrown. "))
                                               (list)
                                               (list)
                                               'no-vararg))
                     ;(hash (list)) (list) 'no-vararg (CPrim1 'print (CStr "printing")) (list) -1))
                     (values (CStr "__name__") (CStr "Exception")))) (cType "class" (CNone))))

(define testObj
  (CHash (hash (list)) (cType "Object" (CId 'TestClass))))

;;STILL TO DO: assertRaises and fail
(define lib-functions
  (list (bind 'print print-lambda)
        (bind 'True true-val)
        (bind '___assertTrue assert-true-lambda)
        (bind '___assertFalse assert-false-lambda)
        (bind '___assertIn assert-in-lambda)
        (bind '___assertNotIn assert-notIn-lambda)
        (bind '___assertEqual assert-equal-lambda)
        (bind '___assertNotEqual assert-notEqual-lambda)
        (bind '___assertIs assert-is-lambda)
        (bind '___assertIsNot assert-isNot-lambda)
        (bind '___assertRaises assert-raises-lambda)
        (bind '___fail python-fail)
        (bind 'python-add python-add)
        (bind 'python-sub python-sub)
        (bind 'python-mult python-mult)
        (bind 'python-div python-div)
        (bind 'python-floor-div python-floor-div)
        (bind 'python-mod python-mod)
        (bind 'python-lt python-lt)
        (bind 'python-lte python-lte)
        (bind 'python-gt python-gt)
        (bind 'python-gte python-gte)
        (bind 'python-eq python-eq)
        (bind 'python-notEq python-notEq)
        (bind 'python-is python-is)
        (bind 'python-isNot python-isNot)
        (bind 'python-in python-in)
        (bind 'python-notIn python-notIn)
        (bind 'len len)
        (bind 'abs abs)
       ; (bind 'bool bool)
        (bind 'bool bool-primitive-class)
       ; (bind 'str str)
        (bind 'str str-primitive-class)
       ; (bind 'float float)
        (bind 'float float-primitive-class)
       ; (bind 'int int)
        (bind 'int int-primitive-class)
       ; (bind 'list make-list)
        (bind 'list list-primitive-class)
       ; (bind 'tuple make-tuple)
        (bind 'tuple (CNone))
        (bind 'tuple tuple-primitive-class)
        (bind 'dict dict-primitive-class)
        (bind 'callable callable)
        (bind 'range make-range)
        (bind 'python-make-range python-make-range)
        (bind 'python-uadd python-uadd)
        (bind 'python-invert python-invert)
        (bind 'print print)
        (bind 'python-not python-not)
        (bind 'python-negate python-negate)
        (bind 'True (CTrue)) ;; not entirely sure these should be here, but we're passing more tests now...
        (bind 'False (CFalse))
        (bind 'create-global-env create-global-env)
        (bind 'isinstance python-isinstance)
        
        ;; exceptions (prelim...)
        (bind 'TypeError type-error-def)
        (bind 'IndexError index-error-def)
        (bind 'ZeroDivisionError zero-division-error)
        (bind 'KeyError key-error)
        (bind 'RuntimeError runtime-error)
        (bind 'UnboundLocalError unbound-local-error)
        (bind 'NameError name-error)
        
        ;;binding of built-in classes
        (bind 'Exception Exception)
        (bind 'TestClass TestClass)
        
        
        
        ;; Some more permanent builtin classes
      ;;  (bind 'ZeroDivisionError zero-division-error)
        
        ;;object for debugging
        (bind 'testObj testObj)

))


(define (python-lib expr)
  (local [(define (python-lib/recur libs)
            (cond [(empty? libs) expr]
                  [(cons? libs)
                   (type-case LibBinding (first libs)
                     (bind (name value)
                           (CLet name (Local) value
                                 (python-lib/recur (rest libs)))))]))]
    (python-lib/recur lib-functions)))


