#lang plai-typed

(require "python-core-syntax.rkt"
         "python-primitives.rkt")

(require (typed-in racket/base [string<? : [string string -> boolean]]))
(require (typed-in racket/base [string>? : [string string -> boolean]]))
(require (typed-in racket/base [string<=? : [string string -> boolean]]))
(require (typed-in racket/base [string>=? : [string string -> boolean]]))
(require (typed-in racket/base [string-length : [string -> number]]))

;;Returns a new memory address to be used
(define new-loc
  (let ([n (box 0)])
    (lambda ()
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))

;;this is the global variable with the global environment
(define globalEnv
  (hash (list)))
   
;;this should be called only once, at the beggining of the interpretation, 
;;to copy the initial environment and create the global one
(define (createGlobalEnv [env : Env]) : Env
  (foldl (lambda (key newEnv) 
           (type-case (optionof SLTuple) (hash-ref env key)
             [none () (error 'createGlobalScope "Cannot find key inside hash with this key in hash-keys: something is very wrong")]
             [some (v) (local [(define-values (t l) v)]
                         (augmentEnv key (values (Global) l) newEnv))]))
         (hash (list))
         (hash-keys env)))

;;newEnvScope returns an environment with the changes needed for a new scope.
;;It basically changes the local tags to nonlocal ones.
(define (newEnvScope [env : Env]) : Env
  (foldl (lambda (key newEnv) 
           (type-case (optionof SLTuple) (hash-ref env key)
             [none () (error 'newEnvScope "Cannot find key inside hash with this key in hash-keys: something is very wrong")]
             [some (v) (local [(define-values (t l) v)]
                         (type-case ScopeType t
                           [Local () (augmentEnv key (values (NonLocal) l) newEnv)]
                           [Global () newEnv]
                           [NonLocal () (augmentEnv key (values (NonLocal) l) newEnv)]))]))
         (hash (list))
         (hash-keys env)))
      
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
    [some (v) (type-case CVal v
                [VUnbound () (error 'lookupStore "Unbound Identifier: using identifier before assignment")]
                [else v])]))

;;helper method for our interpreter
(define (interp-args-CApp [body : CExp]
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
(define (interp-CApp [body : CExp]
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


;; tagof wrapper
(define (interp-tagof [arg : CExp] [env : Env] [store : Store]) : AnswerC
  (type-case AnswerC (interp-env arg env store)
    [ValueA (v s) (ValueA (VStr (get-tag v)) s)]))


;; This is the tagof operator that we will need later...
(define (get-tag [val : CVal]) : string
  (type-case CVal val
    [VNum (n) "int"] ;; this really should distinguish ints from floats...
          ;(cond
              ;  [(fixnum? n) "int"]
              ;  [(flonum? n) "float"])]
    [VStr (s) "string"]
    [VClosure (e a b) "function"]
    [VTrue () "bool"]
    [VFalse () "bool"]
    [VNone () "NoneType"] ;; TODO this looks like a class name. Maybe we should make it so?
    [VPass () "pass"] ;; should never be reached. 
    [VUnbound () "unbound"])) ;; should never be reached. 


;; This is going to be an interp function that works on arbitrary CExps.
;; interp-binop
(define (interp-binop [op : symbol] [e1 : CExp] [e2 : CExp] [env : Env] [store : Store]) : AnswerC
  (type-case AnswerC (interp-env e1 env store)
             [ValueA (v1 s1) (type-case AnswerC (interp-env e2 env s1)
                               [ValueA (v2 s2) (ValueA (handle-op op v1 v2) s2)])]))



;; this function handles binary operations
;; it does NO TYPE CHECKING! We will need to check types in library functions. 
; another case for the library functions: where else do we put the errors? 
;; if we have regular exceptions, we will need to throw them higher up...
;; Need a "tagof" unary operator. Doesn't python have "type"?

;; We need separate float and intger values. 

;; Also, this function should be in the "primitives" file. 
(define (handle-op [op : symbol] [v1 : CVal] [v2 : CVal]) : CVal
  (case op
    ['eq (if (equal? v1 v2) (VTrue) (VFalse))]
    ['notEq (if (equal? v1 v2) (VFalse) (VTrue))]
    ['num+ (VNum (+ (VNum-n v1) (VNum-n v2)))]
    ['string+ (VStr (string-append (VStr-s v1) (VStr-s v2)))]
    ['num- (VNum (- (VNum-n v1) (VNum-n v2)))]
    ['num* (VNum (* (VNum-n v1) (VNum-n v2)))]
    ['num/ (VNum (/ (VNum-n v1) (VNum-n v2)))]
    ['num-lt (if (< (VNum-n v1) (VNum-n v2)) (VTrue) (VFalse))]
    ['string-lt (if (string<? (VStr-s v1) (VStr-s v2)) (VTrue) (VFalse))]
    ['num-lte (if (<= (VNum-n v1) (VNum-n v2)) (VTrue) (VFalse))]
    ['string-lte (if (string<=? (VStr-s v1) (VStr-s v2)) (VTrue) (VFalse))]
    ['num-gt (if (> (VNum-n v1) (VNum-n v2)) (VTrue) (VFalse))]
    ['string-gt (if (string>? (VStr-s v1) (VStr-s v2)) (VTrue) (VFalse))]
    ['num-gte (if (>= (VNum-n v1) (VNum-n v2)) (VTrue) (VFalse))]
    ['string-gte (if (string>=? (VStr-s v1) (VStr-s v2)) (VTrue) (VFalse))]
    
  ;  ['lt (type-case CVal v1
  ;          [VNum (n1) (type-case CVal v2
  ;                       [VNum (n2) (if (< n1 n2) (VTrue) (VFalse))]
  ;                       [else (error 'interp-lt "comparison not valid for arguments of different types")])]
  ;          [VStr (n1) (type-case CVal v2
  ;                       [VStr (n2) (if (string<? n1 n2) (VTrue) (VFalse))]
  ;                       [else (error 'interp-lt "comparison not valid for arguments of different types")])]
  ;          [else (error 'interp-lt "comparison not valid for arguments of this type")])]
  ;  ['lte (type-case CVal v1
  ;          [VNum (n1) (type-case CVal v2
  ;                       [VNum (n2) (if (<= n1 n2) (VTrue) (VFalse))]
  ;                       [else (error 'interp-lte "comparison not valid for arguments of different types")])]
  ;          [VStr (n1) (type-case CVal v2
  ;                       [VStr (n2) (if (string<=? n1 n2) (VTrue) (VFalse))]
  ;                       [else (error 'interp-lte "comparison not valid for arguments of different types")])]
  ;          [else (error 'interp-lte "comparison not valid for arguments of this type")])]
 ;   ['gt (type-case CVal v1
 ;           [VNum (n1) (type-case CVal v2
  ;                       [VNum (n2) (if (> n1 n2) (VTrue) (VFalse))]
 ;                        [else (error 'interp-gt "comparison not valid for arguments of different types")])]
 ;           [VStr (n1) (type-case CVal v2
 ;                        [VStr (n2) (if (string>? n1 n2) (VTrue) (VFalse))]
  ;                       [else (error 'interp-gt "comparison not valid for arguments of different types")])]
  ;          [else (error 'interp-gt "comparison not valid for arguments of this type")])]
  ;  ['gte (type-case CVal v1
  ;          [VNum (n1) (type-case CVal v2
  ;                       [VNum (n2) (if (>= n1 n2) (VTrue) (VFalse))]
  ;                       [else (error 'interp-gte "comparison not valid for arguments of different types")])]
   ;         [VStr (n1) (type-case CVal v2
  ;                       [VStr (n2) (if (string>=? n1 n2) (VTrue) (VFalse))]
 ;                        [else (error 'interp-gte "comparison not valid for arguments of different types")])]
;            [else (error 'interp-gte "comparison not valid for arguments of this type")])]
    [else (error 'handle-op "case not implemented")]))


      ;;        ['eq (interp-eq e1 e2 env store)]
      ;        ['notEq (interp-notEq e1 e2 env store)]
      ;        ['lt (interp-lt e1 e2 env store)]
      ;        ['lte (interp-lte e1 e2 env store)]
      ;        ['gt (interp-gt e1 e2 env store)]
      ;        ['gte (interp-gte e1 e2 env store)]
      ;        ['is (interp-is e1 e2 env store)]
      ;        ['isNot (interp-isNot e1 e2 env store)]
      ;        ['in (interp-in e1 e2 env store)]
      ;        ;;binops
      ;        ['add (interp-add e1 e2 env store)]
      ;        ['sub (interp-sub e1 e2 env store)]
      ;        ['mult (interp-mult e1 e2 env store)]
      ;        ['div (interp-div e1 e2 env store)]
       ;       [else (error 'interp "Invalid CPrim2 operation")]
       ;       )]



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

;;and returns e1 if its value is not truthy; else, 
;;returns e2's value
(define (interp-and [e1 : CExp]
                   [e2 : CExp]
                   [env : Env]
                   [store : Store]) : AnswerC
  (type-case AnswerC (interp-env e1 env store)
    [ValueA (v s) (if (not (isTruthy v))
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
            [VStr (n1) (type-case CVal v2
                         [VStr (n2) (if (string<? n1 n2)
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
            [VStr (n1) (type-case CVal v2
                         [VStr (n2) (if (string>? n1 n2)
                                        (ValueA (VTrue) s2)
                                        (ValueA (VFalse) s2))]
                         [else (error 'interp-lt "comparison not valid for arguments of different types")])]
            [else (error 'interp-gt "comparison not valid for arguments of this type")])])]))

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
            [VStr (n1) (type-case CVal v2
                         [VStr (n2) (if (string<=? n1 n2)
                                        (ValueA (VTrue) s2)
                                        (ValueA (VFalse) s2))]
                         [else (error 'interp-lte "comparison not valid for arguments of different types")])]
            [else (error 'interp-lte "comparison not valid for arguments of this type")])])]))

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
            [VStr (n1) (type-case CVal v2
                         [VStr (n2) (if (string>=? n1 n2)
                                        (ValueA (VTrue) s2)
                                        (ValueA (VFalse) s2))]
                         [else (error 'interp-lt "comparison not valid for arguments of different types")])]
            [else (error 'interp-gte "comparison not valid for arguments of this type")])])]))

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
            [VNone () (type-case CVal v2
                        [VNone () (ValueA (VTrue) s2)]
                        [else (ValueA (VFalse) s2)])]
            [VTrue () (type-case CVal v2
                        [VTrue () (ValueA (VTrue) s2)]
                        [else (ValueA (VFalse) s2)])]
            [VFalse () (type-case CVal v2
                        [VFalse () (ValueA (VTrue) s2)]
                        [else (ValueA (VFalse) s2)])]
            [else (error 'interp-is "comparison not valid for arguments of this type")])])]))

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
            [else (error 'interp-isNot (string-append "comparison not valid for arguments of this type" 
                                                   (string-append (to-string v1) (to-string v2))))])])]))

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


;; interp-print
(define (interp-print (arg : CExp) (env : Env) (store : Store)) : AnswerC
  (type-case AnswerC (interp-env arg env store)
    [ValueA (v s) (begin (display (pretty v))
                         (ValueA v s))]))


;; interp-not
(define (interp-not (arg : CExp) (env : Env) (store : Store)) : AnswerC
  (type-case AnswerC (interp-env arg env store)
    [ValueA (v s) (if (isTruthy v)
                      (ValueA (VFalse) s)
                      (ValueA (VTrue) s))]))

;; interp-negative
(define (interp-negative (arg : CExp) (env : Env) (store : Store)) : AnswerC
  (type-case AnswerC (interp-env arg env store)
    [ValueA (v s) (type-case CVal v
                    [VNum (n) (ValueA (VNum (- 0 n)) s)] ;; gotta be a better way...
                    [else (error 'interp "Tried to negate a non-number")])]))


;; interp-in
(define (interp-in (left : CExp) (right : CExp) (env : Env) (store : Store)) : AnswerC
  (type-case AnswerC (interp-env left env store)
    [ValueA (v1 s1)
            (type-case AnswerC (interp-env right env s1)
              [ValueA (v2 s2)
                      (type-case CVal v1
                        [VStr (str1) (type-case CVal v2
                                       [VStr (str2) (if false ;; False, so that it typechecks. Need actual
                                                        (ValueA (VTrue) s2) ;; condition. 
                                                        (ValueA (VFalse) s2))]
                                       [else (error 'interp-in "\"in\" not valid for these (differing?) types")])]
                        [else (error 'interp-in "\"in\" is not valid for arguments of this type (yet?)")])])]))

;;interp-add
(define (interp-add [left : CExp] 
                    [right : CExp] 
                    [env : Env] 
                    [store : Store]) : AnswerC
  (type-case AnswerC (interp-env left env store)
    [ValueA (v1 s1)
      (type-case AnswerC (interp-env right env s1)
        [ValueA (v2 s2)
          (type-case CVal v1
            [VNum (n1) (type-case CVal v2
                         [VNum (n2) (ValueA (VNum (+ n1 n2)) s2)]
                         [else (error 'interp-add "Cannot add two different types")])]
            [else (error 'interp-gte "Addition not valid for arguments of this type")])])]))

;;interp-sub
(define (interp-sub [left : CExp] 
                    [right : CExp] 
                    [env : Env] 
                    [store : Store]) : AnswerC
  (type-case AnswerC (interp-env left env store)
    [ValueA (v1 s1)
      (type-case AnswerC (interp-env right env s1)
        [ValueA (v2 s2)
          (type-case CVal v1
            [VNum (n1) (type-case CVal v2
                         [VNum (n2) (ValueA (VNum (- n1 n2)) s2)]
                         [else (error 'interp-sub "Cannot subtract two different types")])]
            [else (error 'interp-gte "Subtraction not valid for arguments of this type")])])]))

;;interp-mult
(define (interp-mult [left : CExp] 
                    [right : CExp] 
                    [env : Env] 
                    [store : Store]) : AnswerC
  (type-case AnswerC (interp-env left env store)
    [ValueA (v1 s1)
      (type-case AnswerC (interp-env right env s1)
        [ValueA (v2 s2)
          (type-case CVal v1
            [VNum (n1) (type-case CVal v2
                         [VNum (n2) (ValueA (VNum (* n1 n2)) s2)]
                         [else (error 'interp-mult "Cannot multiply two different types")])]
            [else (error 'interp-gte "Multiplication not valid for arguments of this type")])])]))

;;interp-div
(define (interp-div [left : CExp] 
                    [right : CExp] 
                    [env : Env] 
                    [store : Store]) : AnswerC
  (type-case AnswerC (interp-env left env store)
    [ValueA (v1 s1)
      (type-case AnswerC (interp-env right env s1)
        [ValueA (v2 s2)
          (type-case CVal v1
            [VNum (n1) (type-case CVal v2
                         [VNum (n2) (if (= n2 0)
                                        (error 'divide-by-zero "Cannot divide by zero")
                                        (ValueA (VNum (/ n1 n2)) s2))]
                         [else (error 'interp-div "Cannot divide two different types")])]
            [else (error 'interp-gte "Division not valid for arguments of this type")])])]))

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
    [VStr (s)
          (if (> (string-length s) 0)
              true
              false)]
    [else false]))


;; This function implements the builtin "length" function for strings. 
;; It will eventually be factored out into the library, I hope. 
;; that is to say, this is the primitive.
;; It should move to python-primitives. 
;; It does perform type checking, but shouldn't have to eventually. 
(define (interp-length [expr : CExp] [env : Env] [store : Store]) : AnswerC
  (type-case AnswerC (interp-env expr env store)
    [ValueA (v s) (type-case CVal v
                    [VStr (str) (ValueA (VNum (string-length str)) s)]
                    [else (error 'interp-length "Should only be called on strings.")])]))

;; same here. Just putting it here for the ego boost of passing tests. 
(define (interp-to-bool [expr : CExp] [env : Env] [store : Store]) : AnswerC
  (type-case AnswerC (interp-env expr env store)
    [ValueA (v s) (ValueA (if (isTruthy v) (VTrue) (VFalse)) s)]))

;; and here
(define (interp-to-string [expr : CExp] [env : Env] [store : Store]) : AnswerC
  (type-case AnswerC (interp-env expr env store)
    [ValueA (v s) (ValueA (VStr (pretty v)) s)]))

(define (interp-to-int [expr : CExp] [env : Env] [store : Store]) : AnswerC
  (type-case AnswerC (interp-env expr env store)
    [ValueA (v s) 
            (ValueA (type-case CVal v
              [VFalse () (VNum 0)]
              [VTrue () (VNum 1)]
              [VNum (n) (VNum n)]
              [VStr (s) (error 'interp-to-num "String to Num not implemented yet.")]
              [else (error 'interp-to-num "Should not be called on this type.")]) s)]))

(define (interp-to-float [expr : CExp] [env : Env] [store : Store]) : AnswerC
  (type-case AnswerC (interp-env expr env store)
    [ValueA (v s) 
            (ValueA (type-case CVal v
              [VFalse () (VNum 0.0)]
              [VTrue () (VNum 1.0)]
              [VNum (n) (VNum n)]
              [VStr (s) (error 'interp-to-num "String to Num not implemented yet.")]
              [else (error 'interp-to-num "Should not be called on this type.")]) s)]))


;; interp-env
(define (interp-env [expr : CExp] 
                    [env : Env] 
                    [store : Store]) : AnswerC
  (type-case CExp expr
    [CNum (n) (ValueA (VNum n) store)]
    [CStr (s) (ValueA (VStr s) store)]
    [CTrue () (ValueA (VTrue) store)]

    [CError (e) (error 'interp (pretty (ValueA-value (interp-env e env store))))]
    

    [CId (x) 
         (ValueA (lookupStore (lookupEnv x env) store) store)]

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

    [CSet (id value)
          (type-case CExp id
            [CId (id-symbol) (type-case AnswerC (interp-env value env store)
                               [ValueA (v s)
                                       (ValueA v (augmentStore (lookupEnv id-symbol env)
                                                               v
                                                               s))])]
            [else (error 'interp-CSet "For now, CSet only support ids that are symbols")])]
    
    [CApp (func args)
     (type-case AnswerC (interp-env func env store)
       [ValueA (vf sf)
         (type-case CVal vf
           [VClosure (e a b)
                     (interp-args-CApp b   ;;GOTTA CHANGE THIS TO SOMETHING RELATED TO THE NEW SCOPE!!!!!!!!!!!!!!!!
                                       env
                                       e
                                       sf
                                       a
                                       args
                                       (list))]
           [else (error 'CApp (string-append "Applied a non-function: " (pretty vf)))])])]
    #|
    (type-case CVal (interp-env fun env)
       [VClosure (env argxs body)
         (local [(define argvs (map (lambda (e) (interp-env e env)) arges))]
          (interp-env body (bind-args argxs argvs env)))]
       [else (error 'interp "Not a closure")])]
    |#

    [CFunc (args body) (ValueA (VClosure (newEnvScope env) args body) store)]

    [CPrim1 (prim arg)
            (case prim
              ['print (interp-print arg env store)]
              ['not (interp-not arg env store)]
              ['negative (interp-negative arg env store)]
              ['tagof (interp-tagof arg env store)]
              ['length (interp-length arg env store)]
              ['to-bool (interp-to-bool arg env store)]
              ['to-string (interp-to-string arg env store)]
              ['to-float (interp-to-float arg env store)]
              ['to-int (interp-to-int arg env store)])]
     
     ;; (prim arg) (interp-prim1 prim (interp-env arg env store))]
    
    ;;UNDER THIS, WE HAVE NON-TA CODE:
    [CPrim2 (op e1 e2)
      ;      (begin (display env)
            (case op
              ;;boolops
              ;; These short-circuit, and so need their own system...
              ['or (interp-or e1 e2 env store)]
              ['and (interp-and e1 e2 env store)]
              
              ['is (interp-is e1 e2 env store)] ;; might want to think about these...
              ['isNot (interp-isNot e1 e2 env store)]
              ['in (interp-in e1 e2 env store)]
              
              [else (interp-binop op e1 e2 env store)])
           ; )
            ]
      ;        ;;cmpops
      ;;        ['eq (interp-eq e1 e2 env store)]
      ;        ['notEq (interp-notEq e1 e2 env store)]
      ;        ['lt (interp-lt e1 e2 env store)]
      ;        ['lte (interp-lte e1 e2 env store)]
      ;        ['gt (interp-gt e1 e2 env store)]
      ;        ['gte (interp-gte e1 e2 env store)]
      ;        ['is (interp-is e1 e2 env store)]
      ;        ['isNot (interp-isNot e1 e2 env store)]
      ;        ['in (interp-in e1 e2 env store)]
      ;        ;;binops
      ;        ['add (interp-add e1 e2 env store)]
      ;        ['sub (interp-sub e1 e2 env store)]
      ;        ['mult (interp-mult e1 e2 env store)]
      ;        ['div (interp-div e1 e2 env store)]
       ;       [else (error 'interp "Invalid CPrim2 operation")]
       ;       )]
    [CIf (i t e)
         (type-case AnswerC (interp-env i env store)
           [ValueA (v s)
                   (if (isTruthy v)
                       (interp-env t env s)
                       (interp-env e env s))])]
    [CNone () (ValueA (VNone) store)]
    [CFalse () (ValueA (VFalse) store)] 
    [CPass () (ValueA (VNone) store)] ;; doing nothing. We need a case for that...
    [CUnbound () (ValueA (VUnbound) store)]
    [CGlobalEnv () 
                (begin
                  (set! globalEnv (createGlobalEnv env))
                  (ValueA (VNone) store))]
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


;; regular interpret
(define (interp (expr : CExp)) : CVal
  (type-case AnswerC (interp-env expr (hash (list)) (hash (list)))
    [ValueA (v s) v]))
            
            ;;(if (VPass? v)
                   ;   (void)
                   ;   (print v))]))


;; basic test cases
;;(interp (CTrue))

(define env (hash (list (values 'a (values (Local) 1)) (values 'b (values (NonLocal) 2)) (values 'c (values (Global) 3)))))
