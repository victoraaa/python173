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
    (CPrim1 'print (CId 'to-print))))

(define assert-equal-lambda
  (CFunc (list 'e1 'e2)
    (CIf (CPrim2 'Eq (CId 'e1) (CId 'e2)) 
         (CTrue) 
         (CError (CStr "Assert failed: values are not Equal"))
         )))

(define assert-notEqual-lambda
  (CFunc (list 'e1 'e2)
    (CIf (CPrim2 'Eq (CId 'e1) (CId 'e2))  
         (CError (CStr "Assert failed: values are Equal"))
         (CTrue)
         )))

(define assert-true-lambda
  (CFunc (list 'check-true)
    (CIf (CId 'check-true) (CTrue) (CError (CStr "Assert failed: value is False")))))

(define assert-false-lambda
  (CFunc (list 'check-false)
    (CIf (CId 'check-false) (CError (CStr "Assert failed: value is True")) (CTrue) )))

(define assert-is-lambda
  (CFunc (list 'e1 'e2)
    (CIf (CPrim2 'Is (CId 'e1) (CId 'e2)) 
         (CTrue) 
         (CError (CStr "Assert failed: first argument is not second argument"))
         )))

(define assert-in-lambda
  (CFunc (list 'e1 'e2)
    (CIf (CPrim2 'In (CId 'e1) (CId 'e2)) 
         (CTrue) 
         (CError (CStr "Assert failed: element not found"))
         )))

(define assert-notIn-lambda
  (CFunc (list 'e1 'e2)
    (CIf (CPrim2 'Is (CId 'e1) (CId 'e2)) 
         (CError (CStr "Assert failed: element found"))
         (CTrue)
         )))

(define true-val
  (CTrue))

(define-type LibBinding
  [bind (left : symbol) (right : CExp)])

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

))

(define (python-lib expr)
  (local [(define (python-lib/recur libs)
            (cond [(empty? libs) expr]
                  [(cons? libs)
                   (type-case LibBinding (first libs)
                     (bind (name value)
                           (CLet name value
                                 (python-lib/recur (rest libs)))))]))]
    (python-lib/recur lib-functions)))


