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
    (CPrim1 'print (CId 'to-print)) (list) (list)))

(define assert-equal-lambda
  (CFunc (list 'e-1 'e-2)
    (CIf (CApp (CId 'python-eq) (list (CId 'e-1) (CId 'e-2)))
         (CPass) 
         (CError (CStr "Assert failed: values are not Equal"))
         ) 
    (list)
    (list)))

(define assert-notEqual-lambda
  (CFunc (list 'e-1 'e-2)
    (CIf (CPrim2 'eq (CId 'e-1) (CId 'e-2))  
         (CError (CStr "Assert failed: values are Equal"))
         (CPass)
         )
    (list)
    (list)))

(define assert-true-lambda
  (CFunc (list 'check-true)
    (CIf (CId 'check-true) (CPass) (CError (CStr "Assert failed: value is False")))
    (list)
    (list)))

(define assert-false-lambda
  (CFunc (list 'check-false)
    (CIf (CId 'check-false) (CError (CStr "Assert failed: value is True")) (CPass) )
    (list)
    (list)))

(define assert-is-lambda
  (CFunc (list 'e-1 'e-2)
    (CIf (CPrim2 'is (CId 'e-1) (CId 'e-2)) 
         (CPass) 
         (CError (CStr "Assert failed: first argument is not second argument"))
         )
    (list)
    (list)))

(define assert-isNot-lambda
  (CFunc (list 'e-1 'e-2)
    (CIf (CPrim2 'is (CId 'e-1) (CId 'e-2)) 
         (CError (CStr "Assert failed: first argument is second argument"))
         (CPass) 
         )
    (list)
    (list)))

(define assert-in-lambda
  (CFunc (list 'e-1 'e-2)
    (CIf (CPrim2 'in (CId 'e-1) (CId 'e-2)) 
         (CPass) 
         (CError (CStr "Assert failed: element not found"))
         )
    (list)
    (list)))

(define assert-notIn-lambda
  (CFunc (list 'e-1 'e-2)
    (CIf (CPrim2 'is (CId 'e-1) (CId 'e-2)) 
         (CError (CStr "Assert failed: element found"))
         (CPass)
         )
    (list)
    (list)))

;(define assert-raises-lambda
 ; (CFunc


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
         (list)))


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
         (list)))

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
         (list)))

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
              (CIf (CPrim2 'eq (CPrim1 'to-float (CId 'e-2)) (CNum 0))
                   (CError (CStr "/: Divide by zero"))
                   (CPrim2 'num/ (CPrim1 'to-float (CId 'e-1)) (CPrim1 'to-float (CId 'e-2))))
              (CError (CStr "/: Not supported for this type.")))
         (list)
         (list)))



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
         (list)))

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
         (list)))

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
         (list)))

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
         (list)))

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
         (list)))

;; TODO: need invert, negate, and not cases. With typechecking. 


(define python-invert
  (CFunc (list 'e-1)
         (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
              (CPrim1 'invert (CId 'e-1))
              (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "bool"))
                   (CPrim1 'invert (CPrim1 'to-int (CId 'e-1)))
                   (CError (CStr "~: Cannot invert this type."))))
         (list)
         (list)))


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
         (list)))

(define python-notEq
  (CFunc (list 'e-1 'e-2)
         (CPrim2 'notEq (CId 'e-1) (CId 'e-2))
         (list)
         (list)))

(define python-is
  (CFunc (list 'e-1 'e-2)
         (CPrim2 'is (CId 'e-1) (CId 'e-2))
         (list)
         (list)))

(define python-isNot
  (CFunc (list 'e-1 'e-2)
         (CPrim1 'not (CPrim2 'is (CId 'e-1) (CId 'e-2)))
         (list)
         (list)))

(define python-in
  (CFunc (list 'e-1 'e-2)
         (CPrim2 'in (CId 'e-1) (CId 'e-2))
         (list)
         (list)))

(define python-notIn
  (CFunc (list 'e-1 'e-2)
         (CPrim1 'not (CPrim2 'in (CId 'e-1) (CId 'e-2)))
         (list)
         (list)))

(define print
  (CFunc (list 'e-1)
         (CPrim1 'print (CId 'e-1))
         (list)
         (list)))

(define python-not
  (CFunc (list 'e-1)
         (CPrim1 'not (CId 'e-1))
         (list)
         (list)))

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
         (list)))

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
         (list)))

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
         (list)))

;; Callable
;; may need to re-write this in the future - it depends. I don't know yet. 
(define callable
  (CFunc (list 'e-1)
         (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "function"))
              (CTrue)
              (CFalse))
         (list)
         (list)))

(define bool ;; needs to handle arbitrary-arity input
  (CFunc (list 'e-1)
         (CPrim1 'to-bool (CId 'e-1))
         (list)
         (list (CFalse))))

(define str
  (CFunc (list 'e-1)
         (CPrim1 'to-string (CId 'e-1))
         (list)
         (list)))

(define float
  (CFunc (list 'e-1)
         (CPrim1 'to-float (CId 'e-1))
         (list)
         (list)))

(define int
  (CFunc (list 'e-1)
         (CPrim1 'to-int (CId 'e-1))
         (list)
         (list)))

(define make-list
  (CFunc (list 'e-1)
         (CPrim1 'to-list (CId 'e-1))
         (list)
         (list)))

(define make-tuple
  (CFunc (list 'e-1)
         (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "tuple")) 
              (CId 'e-1)
              (CPrim1 'to-tuple (CId 'e-1)))
         (list)
         (list (CHash (hash (list)) (Type "tuple" (list))))))

(define make-range
  (CFunc (list 'e-1 'e-2 'e-3)
         (CIf (CPrim2 'and 
                      (CPrim2 'is (CId 'e-2) (CNone)) 
                      (CPrim2 'is (CId 'e-3) (CNone)))
              (CApp (CId 'python-make-range) (list (CNum 0) (CId 'e-1) (CNum 1)))
              (CIf (CPrim2 'is (CId 'e-3) (CNone))
                   (CApp (CId 'python-make-range) (list (CId 'e-1) (CId 'e-2) (CNum 1)))
                   (CApp (CId 'python-make-range) (list (CId 'e-1) (CId 'e-2) (CId 'e-3))))) 
         (list)
         (list (CNone) (CNone))))

(define python-make-range
  (CLet 'python-make-range
        (Local)
        (CFunc (list) (CError (CStr "Dummy! (python-make-range)")) (list) (list))
        (CSet (CId 'python-make-range)
              (CFunc (list 'e-1 'e-2 'e-3)
                     (CIf (CPrim2 'eq (CId 'e-1) (CId 'e-2))
                          (CHash (hash (list)) (Type "list" (list)))
                          (CPrim2 'list+ 
                                  (CHash (hash-set (hash (list)) (CNum 0) (CId 'e-1)) (Type "list" (list)))
                                  (CApp (CId 'python-make-range) 
                                        (list (CPrim2 'num+ (CId 'e-1) (CId 'e-3)) (CId 'e-2) (CId 'e-3)))))
                     (list)
                     (list)))))
               
(define create-global-env
  (CFunc (list)
         (CGlobalEnv)
         (list)
         (list)))
         

(define true-val
  (CTrue))

(define-type LibBinding
  [bind (left : symbol) (right : CExp)])

;; Here, we define built-in CLASSES
(define Exception
  (CClass (hash (list)) (Type "Exception" (list))))

(define TestClass
  (CClass (hash (list (values (VStr "f") (VClosure (hash (list)) (list) (CPrim1 'print (CStr "printing")) (list) -1)))) (Type "TestClass" (list))))

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
        (bind 'python-add python-add)
        (bind 'python-sub python-sub)
        (bind 'python-mult python-mult)
        (bind 'python-div python-div)
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
        (bind 'bool bool)
        (bind 'str str)
        (bind 'float float)
        (bind 'int int)
        (bind 'list make-list)
        (bind 'tuple make-tuple)
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
        
        ;;binding of built-in classes
        (bind 'Exception Exception)
        (bind 'TestClass TestClass)

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


