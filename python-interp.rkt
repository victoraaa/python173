#lang plai-typed

(require "python-core-syntax.rkt"
         "python-primitives.rkt")

;;or returns e1 if its value is truthy; if not, 
;;returns e2's value
(define (interp-or [e1 : CExp]
                   [e2 : CExp]
                   [env : Env]) : CVal
  (let ([e1-value (interp-env e1 env)])
    (if (isTruthy e1-value)
        e1-value
        (interp-env e2 env))))

;;eq returns true if both expressions evaluate to the same value
;;it uses the equal? racket operator to compare the values
(define (interp-eq [e1 : CExp]
                   [e2 : CExp]
                   [env : Env]) : CVal
  (let ([e1-value (interp-env e1 env)])
    (let ([e2-value (interp-env e2 env)])
      (if (equal? e1-value e2-value)
          (VTrue)
          (VFalse)))))

;;lt returns true if e1 and e2 are comparable and e1 is lesser than
;;e2
(define (interp-lt [e1 : CExp]
                   [e2 : CExp]
                   [env : Env]) : CVal
  (type-case CVal (interp-env e1 env)
    [VNum (n1) (type-case CVal (interp-env e2 env)
                 [VNum (n2) (if (< n1 n2)
                                (VTrue)
                                (VFalse))]
                [else (error 'interp-lt "comparison not valid for arguments of different types")])]
    [else (error 'interp-lt "comparison not valid for arguments of this type")]))
    
;;gt returns true if e1 and e2 are comparable and e1 is greater than
;;e2
(define (interp-gt [e1 : CExp]
                   [e2 : CExp]
                   [env : Env]) : CVal
  (type-case CVal (interp-env e1 env)
    [VNum (n1) (type-case CVal (interp-env e2 env)
                 [VNum (n2) (if (> n1 n2)
                                (VTrue)
                                (VFalse))]
                [else (error 'interp-lt "comparison not valid for arguments of different types")])]
    [else (error 'interp-lt "comparison not valid for arguments of this type")]))

;;lte returns true if e1 and e2 are comparable and e1 is lesser than
;; or equal to e2
(define (interp-lte [e1 : CExp]
                   [e2 : CExp]
                   [env : Env]) : CVal
  (type-case CVal (interp-env e1 env)
    [VNum (n1) (type-case CVal (interp-env e2 env)
                 [VNum (n2) (if (<= n1 n2)
                                (VTrue)
                                (VFalse))]
                [else (error 'interp-lt "comparison not valid for arguments of different types")])]
    [else (error 'interp-lt "comparison not valid for arguments of this type")]))

;;gte returns true if e1 and e2 are comparable and e1 is greater than
;; or equal to e2
(define (interp-gte [e1 : CExp]
                    [e2 : CExp]
                    [env : Env]) : CVal
  (type-case CVal (interp-env e1 env)
    [VNum (n1) (type-case CVal (interp-env e2 env)
                 [VNum (n2) (if (>= n1 n2)
                                (VTrue)
                                (VFalse))]
                 [else (error 'interp-lt "comparison not valid for arguments of different types")])]
    [else (error 'interp-lt "comparison not valid for arguments of this type")]))

;;is returns true if e1 and e2 are the same object in python
(define (interp-is [e1 : CExp]
                   [e2 : CExp]
                   [env : Env]) : CVal
  (type-case CVal (interp-env e1 env)
    [VNum (n1) (type-case CVal (interp-env e2 env)
                 [VNum (n2) (if (equal? n1 n2)
                                (VTrue)
                                (VFalse))]
                 [else (VFalse)])]
    [else (error 'interp-lt "comparison not yet implemented for arguments of this type")]))

;;'is not' returns true if e1 and e2 are not the same object in python
(define (interp-isNot [e1 : CExp]
                      [e2 : CExp]
                      [env : Env]) : CVal
  (type-case CVal (interp-env e1 env)
    [VNum (n1) (type-case CVal (interp-env e2 env)
                 [VNum (n2) (if (equal? n1 n2)
                                (VFalse)
                                (VTrue))]
                 [else (VTrue)])]
    [else (error 'interp-lt "comparison not yet implemented for arguments of this type")]))


;;interp-notEq returns the opposite of interp-eq. It is a helper
;;for interpreting the NotEq comparison operator
(define (interp-notEq [e1 : CExp]
                      [e2 : CExp]
                      [env : Env]) : CVal
  (type-case CVal (interp-eq e1 e2 env)
    [VTrue () (VFalse)]
    [VFalse () (VTrue)]
    [else (error 'interp-notEq "interp-eq returned a non-boolean value")]))

;; isTruthy returns false if the CVal value is False to python
;; and true otherwise
(define (isTruthy [value : CVal]) : boolean
  ;;JUST A STUB!!!!!!!!!!! - We need to finish this ----------------------------------------------;;;;;;;;;;;;;;;;;;;;;;;;
  (type-case CVal value
    [VTrue () true]
    [VNum (n)
          (if (equal? n 0)
              false
              true)]
    [else false]))

(define (interp-env expr env)
  (type-case CExp expr
    [CNum (n) (VNum n)]
    [CStr (s) (VStr s)]
    [CTrue () (VTrue)]

    [CError (e) (error 'interp (to-string (interp-env e env)))]
    

    [CId (x) (type-case (optionof CVal) (hash-ref env x)
      [some (v) v]
      [none () (error 'interp "Unbound identifier")])]

    [CLet (x bind body)
      (interp-env body (hash-set env x (interp-env bind env)))]

    [CSeq (e1 e2)
      (begin (interp-env e1 env) (interp-env e2 env))]

    [CApp (fun arges)
     (type-case CVal (interp-env fun env)
       [VClosure (env argxs body)
         (local [(define argvs (map (lambda (e) (interp-env e env)) arges))]
          (interp-env body (bind-args argxs argvs env)))]
       [else (error 'interp "Not a closure")])]

    [CFunc (args body) (VClosure env args body)] 

    [CPrim1 (prim arg) (python-prim1 prim (interp-env arg env))]
    
    ;;UNDER THIS, WE HAVE NON-TA CODE:
    [CPrim2 (op e1 e2)
            (case op
              ;;boolops
              ['or
               (interp-or e1 e2 env)]
              ['and (error 'interp-boolop "not implemented")]
              ;;cmpops
              ['eq (interp-eq e1 e2 env)]
              ['notEq (interp-notEq e1 e2 env)]
              ['lt (interp-lt e1 e2 env)]
              ['lte (interp-lte e1 e2 env)]
              ['gt (interp-gt e1 e2 env)]
              ['gte (interp-gte e1 e2 env)]
              ['is (interp-is e1 e2 env)]
              ['isNot (interp-isNot e1 e2 env)]
              )]
    [CIf (i t e)
         (if (isTruthy (interp-env i env))
             (interp-env t env)
             (interp-env e env))]
    [CNone ()
           (VNone)]
    [CFalse () (VFalse)]
    [else (error 'interp (string-append "Haven't implemented a case yet:\n"
                                       (to-string expr)))]
    ))

(define (bind-args args vals env)
  (cond [(and (empty? args) (empty? vals)) env]
        [(or (empty? args) (empty? vals))
         (error 'interp "Arity mismatch")]
        [(and (cons? args) (cons? vals))
         (hash-set (bind-args (rest args) (rest vals) env)
                   (first args) (first vals))]))

(define (interp expr)
  (display (to-string (interp-env expr (hash (list))))))

