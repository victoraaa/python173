#lang plai-typed

(require "python-core-syntax.rkt"
         "python-primitives.rkt")

;;Returns a new memory address to be used
(define new-loc
  (let ([n (box 0)])
    (lambda ()
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))

;;Adds a new identifier to our environment, with its location
(define (augmentEnv [id : symbol]
                    [sltuple : SLTuple]
                    [env : Env]) : Env
  (hash-set env id sltuple))

;;Adds a location and its Value to our Store
(define (augmentStore [location : Location]
                      [value : CVal]
                      [store : Store]) : Store
  (hash-set store location value))


;;lookupEnv searchs the environment for some identifier
(define (lookupEnv [id : symbol]
                   [env : Env]) : Location
  (type-case (optionof SLTuple) (hash-ref env id)
    [none () (error 'lookupEnv (string-append "Unbound indentifier error: " (symbol->string id)))]
    [some (v) (local [(define-values (t l) v)]
                l)]))

;;lookupStore searches the store for an specific location
(define (lookupStore [loc : Location]
                     [store : Store]) : CVal
  (type-case (optionof CVal) (hash-ref store loc)
    [none () (error 'lookupStore "Unbound location error.")]
    [some (v) v]))

;;helper method for our interpreter
(define (interp-args-CApp [body : ExprC]
                          [env : Env]
                          [closEnv : Env]
                          [store : Store]
                          [argsIds : (listof symbol)]
                          [args : (listof CExp)]
                          [interpretedArgs : (listof CVal)]) : AnswerC
  (cond
    [(empty? args) (interp-CApp body
                                env
                                closEnv
                                store
                                argsIds
                                (reverse interpretedArgs))]
    [else 
     (type-case AnswerC (interp-env (first args) env store)
       [ValueA (v s)
               (interp-args-CApp body
                                 env
                                 closEnv
                                 s
                                 argsIds
                                 (rest args)
                                 (cons v interpretedArgs))])]))

;;puts all the identifiers and values in the environment and the store,
;;and applies the body of the closure
(define (interp-CApp [body : ExprC]
                     [env : Env]
                     [closEnv : Env]
                     [store : Store]
                     [argsIds : (listof symbol)]
                     [args : (listof CVal)]) : AnswerC
  (cond
    [(not (equal? (length argsIds) (length args)))
     (error 'interp-AppC "Application failed with arity mismatch")]
    [(empty? args) (interp-env body 
                               closEnv
                               store)]
    [else 
        (let ([newLocation (new-loc)])
          (interp-CApp body
                       env
                       (augmentEnv (first argsIds)
                                   (values (Local) newLocation)  ;;GOTTA CHANGE THIS AUGMENTENV FOR THE METHOD THAT CREATES THE NEW SCOPE
                                   closEnv)
                       (augmentStore newLocation
                                     (first args)
                                     store)
                       (rest argsIds)
                       (rest args)))]))



;;or returns e1 if its value is truthy; if not, 
;;returns e2's value
(define (interp-or [e1 : CExp]
                   [e2 : CExp]
                   [env : Env]
                   [store : Store]) : AnswerC
  (type-case AnswerC (interp-env e1 env store)
    [ValueA (v s) (if (isTruthy v)
                      (ValueA v s)
                      (interp-env e2 env s))]))

;;eq returns true if both expressions evaluate to the same value
;;it uses the equal? racket operator to compare the values
(define (interp-eq [e1 : CExp]
                   [e2 : CExp]
                   [env : Env]
                   [store : Store]) : AnswerC
  (type-case AnswerC (interp-env e1 env store)
    [ValueA (v1 s1)
      (type-case AnswerC (interp-env e2 env s1)
        [ValueA (v2 s2)
                (if (equal? v1 v2)
                    (ValueA (VTrue) s2)
                    (ValueA (VFalse) s2))])]))

;;lt returns true if e1 and e2 are comparable and e1 is lesser than
;;e2
(define (interp-lt [e1 : CExp]
                   [e2 : CExp]
                   [env : Env]
                   [store : Store]) : AnswerC
  (type-case AnswerC (interp-env e1 env store)
    [ValueA (v1 s1)
      (type-case AnswerC (interp-env e2 env s1)
        [ValueA (v2 s2)
          (type-case CVal v1
            [VNum (n1) (type-case CVal v2
                         [VNum (n2) (if (< n1 n2)
                                        (ValueA (VTrue) s2)
                                        (ValueA (VFalse) s2))]
                         [else (error 'interp-lt "comparison not valid for arguments of different types")])]
            [else (error 'interp-lt "comparison not valid for arguments of this type")])])]))
    
;;gt returns true if e1 and e2 are comparable and e1 is greater than
;;e2
(define (interp-gt [e1 : CExp]
                   [e2 : CExp]
                   [env : Env]
                   [store : Store]) : AnswerC
  (type-case AnswerC (interp-env e1 env store)
    [ValueA (v1 s1)
      (type-case AnswerC (interp-env e2 env s1)
        [ValueA (v2 s2)
          (type-case CVal v1
            [VNum (n1) (type-case CVal v2
                         [VNum (n2) (if (> n1 n2)
                                        (ValueA (VTrue) s2)
                                        (ValueA (VFalse) s2))]
                         [else (error 'interp-lt "comparison not valid for arguments of different types")])]
            [else (error 'interp-lt "comparison not valid for arguments of this type")])])]))

;;lte returns true if e1 and e2 are comparable and e1 is lesser than
;; or equal to e2
(define (interp-lte [e1 : CExp]
                    [e2 : CExp]
                    [env : Env]
                    [store : Store]) : AnswerC
  (type-case AnswerC (interp-env e1 env store)
    [ValueA (v1 s1)
      (type-case AnswerC (interp-env e2 env s1)
        [ValueA (v2 s2)
          (type-case CVal v1
            [VNum (n1) (type-case CVal v2
                         [VNum (n2) (if (<= n1 n2)
                                        (ValueA (VTrue) s2)
                                        (ValueA (VFalse) s2))]
                         [else (error 'interp-lt "comparison not valid for arguments of different types")])]
            [else (error 'interp-lt "comparison not valid for arguments of this type")])])]))

;;gte returns true if e1 and e2 are comparable and e1 is greater than
;; or equal to e2
(define (interp-gte [e1 : CExp]
                    [e2 : CExp]
                    [env : Env]
                    [store : Store]) : AnswerC
  (type-case AnswerC (interp-env e1 env store)
    [ValueA (v1 s1)
      (type-case AnswerC (interp-env e2 env s1)
        [ValueA (v2 s2)
          (type-case CVal v1
            [VNum (n1) (type-case CVal v2
                         [VNum (n2) (if (>= n1 n2)
                                        (ValueA (VTrue) s2)
                                        (ValueA (VFalse) s2))]
                         [else (error 'interp-lt "comparison not valid for arguments of different types")])]
            [else (error 'interp-lt "comparison not valid for arguments of this type")])])]))

;;is returns true if e1 and e2 are the same object in python
(define (interp-is [e1 : CExp]
                   [e2 : CExp]
                   [env : Env]
                   [store : Store]) : AnswerC
  (type-case AnswerC (interp-env e1 env store)
    [ValueA (v1 s1)
      (type-case AnswerC (interp-env e2 env s1)
        [ValueA (v2 s2)
          (type-case CVal v1
            [VNum (n1) (type-case CVal v2
                         [VNum (n2) (if (equal? n1 n2)
                                        (ValueA (VTrue) s2)
                                        (ValueA (VFalse) s2))]
                         [else (ValueA (VFalse) s2)])]
            [else (error 'interp-lt "comparison not valid for arguments of this type")])])]))

;;'is not' returns true if e1 and e2 are not the same object in python
(define (interp-isNot [e1 : CExp]
                      [e2 : CExp]
                      [env : Env]
                      [store : Store]) : AnswerC
  (type-case AnswerC (interp-env e1 env store)
    [ValueA (v1 s1)
      (type-case AnswerC (interp-env e2 env s1)
        [ValueA (v2 s2)
          (type-case CVal v1
            [VNum (n1) (type-case CVal v2
                         [VNum (n2) (if (equal? n1 n2)
                                        (ValueA (VFalse) s2)
                                        (ValueA (VTrue) s2))]
                         [else (ValueA (VTrue) s2)])]
            [else (error 'interp-lt "comparison not valid for arguments of this type")])])]))

;;interp-notEq returns the opposite of interp-eq. It is a helper
;;for interpreting the NotEq comparison operator
(define (interp-notEq [e1 : CExp]
                      [e2 : CExp]
                      [env : Env]
                      [store : Store]) : AnswerC
  (type-case AnswerC (interp-eq e1 e2 env store)
    [ValueA (v s)
      (type-case CVal v
        [VTrue () (ValueA (VFalse) s)]
        [VFalse () (ValueA (VTrue) s)]
        [else (error 'interp-notEq "interp-eq returned a non-boolean value")])]))

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

(define (interp-env [expr : CExp] 
                    [env : Env] 
                    [store : Store]) : AnswerC
  (type-case CExp expr
    [CNum (n) (ValueA (VNum n) store)]
    [CStr (s) (ValueA (VStr s) store)]
    [CTrue () (ValueA (VTrue) store)]

    [CError (e) (error 'interp (to-string (interp-env e env store)))]
    

    [CId (x) (ValueA (lookupStore (lookupEnv x env) store) store)]

    [CLet (id scopeType bind body)
      ;(interp-env body (hash-set env id (interp-env bind env)))]
      (type-case AnswerC (interp-env bind env store)
        [ValueA (v s)
                (let ([newLocation (new-loc)])
                  (interp-env body
                              (augmentEnv id (values scopeType newLocation) env)
                              (augmentStore newLocation 
                                            v
                                            s)))])]

    [CSeq (e1 e2)
      (type-case AnswerC (interp-env e1 env store)
        [ValueA (v s)
                (interp-env e2 env s)])]

    [CApp (func args)
     (type-case AnswerC (interp-env func env store)
       [ValueA (vf sf)
         (type-case CVal vf
           [VClosure (e a b)
                     (interp-args-CApp b   ;;GOTTA CHANGE THIS TO SOMETHING RELATED TO THE NEW SCOPE!!!!!!!!!!!!!!!!
                                       env
                                       e
                                       s
                                       a
                                       args
                                       (list))]
           [else (error 'CApp (string-append "Applied a non-function: " (pretty-value vf)))])])]
    #|
    (type-case CVal (interp-env fun env)
       [VClosure (env argxs body)
         (local [(define argvs (map (lambda (e) (interp-env e env)) arges))]
          (interp-env body (bind-args argxs argvs env)))]
       [else (error 'interp "Not a closure")])]
    |#

    [CFunc (args body) (ValueA (VClosure env args body) store)] 

    [CPrim1 (prim arg) (python-prim1 prim (interp-env arg env store))]
    
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

