#lang plai-typed

(require "python-core-syntax.rkt"
         "python-primitives.rkt"
         "python-desugar.rkt")

(require (typed-in racket/base [string<? : [string string -> boolean]]))
(require (typed-in racket/base [string>? : [string string -> boolean]]))
(require (typed-in racket/base [string<=? : [string string -> boolean]]))
(require (typed-in racket/base [string>=? : [string string -> boolean]]))
(require (typed-in racket/base [string-length : [string -> number]]))
(require (typed-in racket/base [bitwise-not : [number -> number]]))
(require (typed-in racket/base [fixnum? : [number -> boolean]]))
(require (typed-in racket/base [flonum? : [number -> boolean]]))
;(require (typed-in racket/string [string-split : [string -> (listof string)]]))
(require (typed-in racket/base [string->list : [string -> (listof string)]]))
(require (typed-in racket/base [list->string : [(listof string) -> string]]))
(require (typed-in racket/base [list-tail : [(listof 'a) number -> (listof 'a)]]))
(require (typed-in racket/list [argmax : [('a -> number) (listof 'a) -> 'a]]))
(require (typed-in racket/list [drop-right : [(listof 'a) number -> (listof 'a)]]))
(require (typed-in racket/list [last : [(listof 'a) -> 'a]]))

;;Returns a new memory address to be used
(define new-loc
  (let ([n (box 0)])
    (lambda ()
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))

;;Returns a new uid to be used
(define new-uid
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

;;keepOldEnv is a helper function that will keep all of the non-global variables of the older scope,
;;remembering to change 'Local' variables into 'NonLocal' ones
(define (keepOldEnv [env : Env]) : Env
  (foldl (lambda (key newEnv) 
           (type-case (optionof SLTuple) (hash-ref env key)
             [none () (error 'keepOldEnv "Cannot find key inside hash with this key in hash-keys: something is very wrong")]
             [some (v) (local [(define-values (t l) v)]
                         (type-case ScopeType t
                           [Local () (augmentEnv key (values (NonLocal) l) newEnv)]
                           [Global () newEnv]
                           [NonLocal () (augmentEnv key (values (NonLocal) l) newEnv)]))]))
         (hash (list))
         (hash-keys env)))

;;addGlobalVars will use the vlist (list of variables and ScopeTypes) to insert the variables declared 
;;as Global in the environment
(define (addGlobalVars [env : Env]
                       [vlist : (listof (ScopeType * symbol))]) : Env
  (cond
    [(empty? vlist) env]
    [else (local [(define-values (t id) (first vlist))]
            (if (Global? t)
                (if (inEnv? id globalEnv)
                    (addGlobalVars (augmentEnv id (values (Global) (lookupEnv id globalEnv)) env) 
                                   (rest vlist))
                    (let ([newLocation (new-loc)])
                      (begin 
                        (set! globalEnv (augmentEnv id
                                                    (values (Global) newLocation)
                                                    env))
                        (addGlobalVars (augmentEnv id 
                                                   (values (Global) newLocation) 
                                                   env)
                                       (rest vlist)))))
                (addGlobalVars env (rest vlist))))]))

;;addNonLocals checks for errors that may be raised by the 'nonlocal' expression and,
;;if there are no errors, returns the same environment
(define (addNonLocals [env : Env]
                      [vlist : (listof (ScopeType * symbol))]) : Env
  (cond
    [(empty? vlist) env]
    [else (local [(define-values (t id) (first vlist))]
            (if (NonLocal? t)
                (if (inEnv? id env)
                    (if (Global? (getScopeType id env))
                        (error 'addNonLocals (string-append "no binding for nonlocal " (string-append (symbol->string id) " found")))
                        (addNonLocals env (rest vlist)))
                    (error 'addNonLocals (string-append "no binding for nonlocal " (string-append (symbol->string id) " found"))))
                (addNonLocals env (rest vlist))))]))

;;addLocals receives a list with Local variables candidates and a list with the variables that are 
;;already declared as global or nonlocal in this scope. It returns an environment with the 
;;appended correct Local variables
(define (addLocals [env : Env]
                   [localList : (listof (ScopeType * symbol))]
                   [othersList : (listof (ScopeType * symbol))]) : Env
  (cond
    [(empty? localList) env]
    [else (local [(define-values (t id) (first localList))]
            (if (foldl (lambda (l result) (or l result))
                       false
                       (map (lambda (st-id) (local [(define-values (t2 id2) st-id)]
                                              (if (equal? id id2)
                                                  true
                                                  false)))
                            othersList))
                (addLocals env (rest localList) othersList)
                (addLocals (augmentEnv id 
                                       (values (Local) (new-loc))
                                       env)
                           (rest localList) 
                           othersList)))]))

;;addArgs just appends the args to a list of (ScopeType * symbol), 
;;with the ScopeType 'Local'
(define (addArgs [lst : (listof (ScopeType * symbol))]
                 [args : (listof symbol)]) : (listof (ScopeType * symbol))
  (cond
    [(empty? args) lst]
    [else (addArgs (append (list (values (Local) (first args))) lst)
                   (rest args))]))

;;newEnvScope returns an environment with the changes needed for a new scope.
;;It basically changes the local tags to nonlocal ones.
(define (newEnvScope [env : Env]
                     [vlist : (listof (ScopeType * symbol))]
                     [args : (listof symbol)]
                     [vararg : symbol]) : Env
  (addLocals (addNonLocals (addGlobalVars (keepOldEnv env) 
                                          vlist)
                           vlist)
             (addArgs (filter (lambda (x) (local [(define-values (t id) x)]
                                            (if (Local? t)
                                                true
                                                false)))
                              vlist)
                      (append args (list vararg)))
             (filter (lambda (x) (local [(define-values (t id) x)]
                                   (if (Local? t)
                                       false
                                       true)))
                     vlist)))
      
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

;;inEnv? searches the environment for some identifier, returning true if
;;the identifier is already there and false otherwise
(define (inEnv? [id : symbol]
                [env : Env]) : boolean
  (type-case (optionof SLTuple) (hash-ref env id)
    [none () false]
    [some (v) true]))

;;getScopeType gets the ScopeType of 'id' in the environment 'env'
(define (getScopeType [id : symbol]
                      [env : Env]) : ScopeType
  (type-case (optionof SLTuple) (hash-ref env id)
    [none () (error 'getScopeType (string-append "Unbound Identifier : " (symbol->string id)))]
    [some (v) (local [(define-values (t l) v)]
                t)]))

;;lookupEnv searches the environment for some identifier
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

;;lookupVar searches for the identifier first at the given environment, then at the globalEnv.
(define (lookupVar [id : symbol]
                   [env : Env]) : Location
  (type-case (optionof SLTuple) (hash-ref env id)
    [none () (lookupEnv id globalEnv)]
    [some (v) (local [(define-values (t l) v)]
                l)]))

;;puts the default args of the functino in the places with CUnbounds
(define (add-default-args [args : (listof CVal)]
                          [defaults : (listof CVal)]) : (listof CVal)
  (cond
    [(empty? args) (list)]
    [else
     (if (equal? (last args) (VUnbound))
         (append (add-default-args (drop-right args 1) 
                                   (drop-right defaults 1)) 
                 (list (last defaults)))
         (append (add-default-args (drop-right args 1)
                                   defaults)
                 (list (last args))))]))

;;helper method for our interpreter
(define (interp-args-CApp [body : CExp]
                          [env : Env]
                          [closEnv : Env]
                          [store : Store]
                          [argsIds : (listof symbol)]
                          [args : (listof CExp)]
                          [interpretedArgs : (listof CVal)]
                          [defargs : (listof CVal)]) : AnswerC
  (cond
    [(empty? args) (interp-CApp body
                              (allocateLocals closEnv)
                              store
                              argsIds
                              (add-default-args (reverse interpretedArgs)
                                                defargs))]
                              #|
                              (append (reverse interpretedArgs)
                                      (list-tail defargs 
                                                 (- (length defargs)
                                                    (- (length argsIds) 
                                                       (length interpretedArgs))))))]     
|#
    [else 
     (type-case AnswerC (interp-env (first args) env store)
       [ValueA (v s)
               (interp-args-CApp body
                                 env
                                 closEnv
                                 s
                                 argsIds
                                 (rest args)
                                 (cons v interpretedArgs)
                                 defargs)]
       [ExceptionA (v s) (ExceptionA v s)]
       [ReturnA (v s) (ReturnA v s)])]))

;;helper method that allocates a new position for all of the local variables in the environment. Used when applying a function, because
;;each time we apply we are using new arguments/locals, not the old ones.
(define (allocateLocals [env : Env]) : Env
  (foldl (lambda (key newEnv) 
           (type-case (optionof SLTuple) (hash-ref env key)
             [none () (error 'allocateLocals "Cannot find key inside hash with this key in hash-keys: something is very wrong")]
             [some (v) (local [(define-values (t l) v)]
                         (type-case ScopeType t
                           [Local () (augmentEnv key (values t (new-loc)) newEnv)]
                           [else (augmentEnv key v newEnv)]))]))
         (hash (list))
         (hash-keys env)))

;;puts all the identifiers and values in the environment and the store,
;;and applies the body of the closure
(define (interp-CApp [body : CExp]
                     [closEnv : Env]
                     [store : Store]
                     [argsIds : (listof symbol)]
                     [args : (listof CVal)]) : AnswerC
  (cond
    [(not (equal? (length argsIds) (length args)))
     (error 'interp-AppC "Application failed with arity mismatch")]
    [(empty? args) (type-case AnswerC (interp-env body 
                                                  closEnv
                                                  store)
                     [ValueA (v s) (ValueA v s)]
                     [ExceptionA (v s) (ExceptionA v s)]
                     [ReturnA (v s) (ValueA v s)])]
    [else 
     (interp-CApp body
                  closEnv
                  (augmentStore (lookupEnv (first argsIds) closEnv)
                                (first args)
                                store)
                  (rest argsIds)
                  (rest args))]))
     #|
     (let ([newLocation (new-loc)])
       (interp-CApp body
                    (augmentEnv (first argsIds)
                                (values (Local) newLocation)
                                closEnv)
                    (augmentStore newLocation
                                  (first args)
                                  store)
                    (rest argsIds)
                    (rest args)))]))
|#

;; tagof wrapper
(define (interp-tagof [arg : CExp] [env : Env] [store : Store]) : AnswerC
  (type-case AnswerC (interp-env arg env store)
    [ValueA (v s) (ValueA (VStr (get-tag v)) s)]
    [ExceptionA (v s) (ExceptionA v s)]
    [ReturnA (v s) (ReturnA v s)]))


;; This is the tagof operator that we will need later...
(define (get-tag [val : CVal]) : string
  (type-case CVal val
    [VNum (n) ;"int"] ;; this really should distinguish ints from floats...
          (cond
                [(fixnum? n) "int"]
                [(flonum? n) "float"])]
    [VStr (s) "string"]
    [VClosure (e a varg b defargs uid) "function"]
    [VTrue () "bool"]
    [VFalse () "bool"]
    [VNone () "NoneType"] ;; TODO this looks like a class name. Maybe we should make it so?
    [VPass () "pass"] ;; should never be reached. 
    [VUnbound () "unbound"]
    ;[VList (elts uid) "list"]
    ;[VDict (elts uid) "dict"]
    ;[VTuple (elts uid) "tuple"]
    [VHash (elts uid type) (Type-name type)]
    [VClass (elts type) (Type-name type)]))


;; This is going to be an interp function that works on arbitrary CExps.
;; interp-binop
(define (interp-binop [op : symbol] [e1 : CExp] [e2 : CExp] [env : Env] [store : Store]) : AnswerC
  (type-case AnswerC (interp-env e1 env store)
    [ValueA (v1 s1) (type-case AnswerC (interp-env e2 env s1)
                      [ValueA (v2 s2) (ValueA (handle-op op v1 v2) s2)]
                      [ExceptionA (v s) (ExceptionA v s)]
                      [ReturnA (v2 s2) (ReturnA v2 s2)])]
    [ExceptionA (v s) (ExceptionA v s)]
    [ReturnA (v1 s1) (ReturnA v1 s1)]))


(define (duplicate-string [str : string] [num : number]) : string
  (cond
    [(flonum? num) (error 'duplicate-string "does not work with floats...")]
    [(<= num 0) ""]
    [else (string-append str (duplicate-string str (- num 1)))]))

;; just a helper...
(define (repeat-list [lst : (listof 'a)] [n : number]) : (listof 'a)
  (cond
    [(= n 0) empty]
    [else (append lst (repeat-list lst (- n 1)))]))


(define (duplicate-tuple [tup : (hashof CVal CVal)] [n : number]) : (hashof CVal CVal)
  (cond
    [(flonum? n) (error 'duplicate-tuple "does not work with floats...")]
    [(<= n 0) (hash (list))]
    [else (make-new-map (map (lambda (x) (VNum x)) 
                             (range (* n (argmax (lambda (x) x) (map (lambda (x) (VNum-n x)) (hash-keys tup))))))
                        (repeat-list (hash-values tup) n))]))

;; Assumes we are dealing with VHashs
(define (merge-python-lists (l1 : CVal) (l2 : CVal)) : (hashof CVal CVal)
  (local ([define h1 (VHash-elts l1)]
          [define h2 (VHash-elts l2)]
          [define keylenh (length (hash-keys h1))])
    (foldl (lambda (x h) (hash-set h (VNum (+ (VNum-n x) keylenh)) 
                                   (type-case (optionof CVal) (hash-ref h2 x)
                                     [some (s) s]
                                     [none () (error 'merge-python-lists "???")])))
           h1
           (hash-keys h2))))

   
   
 ;;  (lambda (x) (hash-set h1 (VNum (+ (VNum-n x) keylenh)) (hash-ref h2 x)))

;(make-new-map (map (lambda (x) (VNum x)) 
 ;                  (range (* n (argmax (lambda (x) x) (map (lambda (x) (VNum-n x)) (hash-keys tup))))))
 ;             (repeat-list (hash-values tup) n))



;; this function handles binary operations
;; it does NO TYPE CHECKING! We will need to check types in library functions. 
; another case for the library functions: where else do we put the errors? 
;; if we have regular exceptions, we will need to throw them higher up...
;; Need a "tagof" unary operator. Doesn't python have "type"?

;; We need separate float and intger values. 

;; check-equality
(define (check-equality [v1 : CVal] [v2 : CVal]) : boolean
  (type-case CVal v1
    [VHash (elts1 uid1 type1) (type-case CVal v2
                                [VHash (elts2 uid2 type2) (if (equal? type1 type2)
                                                              (equal? elts1 elts2) ;; TODO recur!
                                                              false)]
                                [else false])]
    [else (equal? v1 v2)]))


;; Also, this function should be in the "primitives" file. 
(define (handle-op [op : symbol] [v1 : CVal] [v2 : CVal]) : CVal
  (case op
    ['eq (if (check-equality v1 v2) (VTrue) (VFalse))]
    ['notEq (if (check-equality v1 v2) (VFalse) (VTrue))]
    ['num+ (VNum (+ (VNum-n v1) (VNum-n v2)))]
    ['string+ (VStr (string-append (VStr-s v1) (VStr-s v2)))]
    ['list+ (VHash (merge-python-lists v1 v2) (new-uid) (Type "list" (list)))]
    ['tuple+ (VHash (merge-python-lists v1 v2) (new-uid) (Type "tuple" (list)))]
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
    ['duplicate (VStr (duplicate-string (VStr-s v1) (VNum-n v2)))] ;; throws exception if types are wrong.
    ['duple (VHash (duplicate-tuple (VHash-elts v1) (VNum-n v2)) (new-uid) (Type "tuple" (list)))]
    [else (error op "handle-op: case not implemented")]))



;;or returns e1 if its value is truthy; if not, 
;;returns e2's value
(define (interp-or [e1 : CExp]
                   [e2 : CExp]
                   [env : Env]
                   [store : Store]) : AnswerC
  (type-case AnswerC (interp-env e1 env store)
    [ValueA (v s) (if (isTruthy v)
                      (ValueA v s)
                      (interp-env e2 env s))]
    [ExceptionA (v s) (ExceptionA v s)]
    [ReturnA (v s) (ReturnA v s)]))

;;and returns e1 if its value is not truthy; else, 
;;returns e2's value
(define (interp-and [e1 : CExp]
                   [e2 : CExp]
                   [env : Env]
                   [store : Store]) : AnswerC
  (type-case AnswerC (interp-env e1 env store)
    [ValueA (v s) (if (not (isTruthy v))
                      (ValueA v s)
                      (interp-env e2 env s))]
    [ExceptionA (v s) (ExceptionA v s)]
    [ReturnA (v s) (ReturnA v s)]))

;;  get-uid returns the uid for any type that has one
(define (get-uid [v : CVal]) : Uid
  (type-case CVal v
    [VClosure (e a varg b defargs uid) uid]
    ;[VList (elts uid) uid]
    ;[VDict (elts uid) uid]
    [VHash (elts uid type) uid]
    [else (error 'get-uid "should not use get-uid for types that do not have Uid's")]))

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
            [VStr (str1) (if (equal? v1 v2)
                             (ValueA (VTrue) s2)
                             (ValueA (VFalse) s2))]
            [else (if (equal? (get-uid v1) (get-uid v2))
                      (ValueA (VTrue) s2)
                      (ValueA (VFalse) s2))])]
        [ExceptionA (v s) (ExceptionA v s)]
        [ReturnA (v2 s2) (ReturnA v2 s2)])]
    [ExceptionA (v s) (ExceptionA v s)]
    [ReturnA (v1 s1) (ReturnA v1 s1)]))

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
                                                   (string-append (to-string v1) (to-string v2))))])]
        [ExceptionA (v s) (ExceptionA v s)]
        [ReturnA (v2 s2) (ReturnA v2 s2)])]
    [ExceptionA (v s) (ExceptionA v s)]
    [ReturnA (v1 s1) (ReturnA v1 s1)]))


;; 
(define (hash-values [h : (hashof 'a 'b)]) : (listof 'b)
  (map (lambda (x) (type-case (optionof 'b) (hash-ref h x)
                     [none () (error 'hash-values "This exists...")]
                     [some (v) v])) (hash-keys h)))


;; interp-in
(define (interp-in (left : CExp) (right : CExp) (env : Env) (store : Store)) : AnswerC
  (type-case AnswerC (interp-env left env store)
    [ValueA (v1 s1)
            (type-case AnswerC (interp-env right env s1)
              [ValueA (v2 s2)
                      (type-case CVal v2
                        [VStr (str2) (type-case CVal v1
                                       [VStr (str1) (if false ;; False, so that it typechecks. Need actual
                                                        (ValueA (VTrue) s2) ;; condition. 
                                                        (ValueA (VFalse) s2))]
                                       [else (error 'interp-in "\"in\" not valid for these (differing?) types")])]
                        #|
                        [VList (elts uid) (if (member v1 (hash-values elts))
                                              (ValueA (VTrue) s2)
                                              (ValueA (VFalse) s2))]
                        [VDict (elts uid) (if (member v1 (hash-keys elts))
                                              (ValueA (VTrue) s2)
                                              (ValueA (VFalse) s2))]
                        |#
                        [VHash (elts uid type) 
                               (cond 
                                 [(equal? (Type-name type) "dict") (if (member v1 (hash-keys elts))
                                                                       (ValueA (VTrue) s2)
                                                                       (ValueA (VFalse) s2))]
                                 [(or (equal? (Type-name type) "list")
                                      (equal? (Type-name type) "tuple")) (if (member v1 (hash-values elts))
                                                                             (ValueA (VTrue) s2)
                                                                             (ValueA (VFalse) s2))])]
                        [else (error 'interp-in "\"in\" is not valid for arguments of this type (yet?)")])]
              [ExceptionA (v s) (ExceptionA v s)]
              [ReturnA (v2 s2) (ReturnA v2 s2)])]
    [ExceptionA (v s) (ExceptionA v s)]
    [ReturnA (v1 s1) (ReturnA v1 s1)]))


;; isTruthy returns false if the CVal value is False to python
;; and true otherwise
(define (isTruthy [value : CVal]) : boolean
  ;;JUST A STUB!!!!!!!!!!! - We need to finish this ----------------------------------------------;;;;;;;;;;;;;;;;;;;;;;;;
  (type-case CVal value
    [VTrue () true]
    [VNum (n)
          (if (= n 0)
              false
              true)]
    [VStr (s)
          (if (> (string-length s) 0)
              true
              false)]
    [VHash (elts uid t)
           (if (empty? (hash-keys elts))
               false
               true)]
    #|
    [VList (elts uid)
           (if (empty? (hash-keys elts))
               false
               true)]
    [VDict (elts uid)
           (if (empty? (hash-keys elts))
               false
               true)]
    |#
    [else false]))


;; TODO should just import these from desugar...
;(define (range [n : number]) : (listof number)
;  (reverse (range-backwards n)))

;(define (range-backwards [n : number]) : (listof number)
;  (cond
;    [(<= n 0) empty]
;    [else (cons (- n 1) (range-backwards (- n 1)))]))

(define (make-new-map [keys : (listof CVal)] [vals : (listof CVal)]) : (hashof CVal CVal)
  (cond 
    [(empty? keys) (hash (list))]
    [(cons? keys) (hash-set (make-new-map (rest keys) (rest vals)) (first keys) (first vals))]))

;; change-string-to-list
;; converts a string into a list
(define (change-string-to-list (s : string)) : (hashof CVal CVal)
  (make-new-map (map (lambda (x) (VNum x)) (range (string-length s))) 
                (map (lambda (x) (VStr (list->string (list x)))) (string->list s))))


;; handle unary operations - akin to handle-op
(define (handle-unary [prim : symbol] [arg : CVal]) : CVal
  (case prim
    ['print (begin (display (pretty arg)) arg)]
    ['not (if (isTruthy arg) (VFalse) (VTrue))]
    ['negative (type-case CVal arg
                    [VNum (n) (VNum (- 0 n))] ;; gotta be a better way...
                    [else (error 'interp "Tried to negate a non-number")])] ;; TODO handle errors outside...
    ['invert (type-case CVal arg
               [VNum (n) (VNum (bitwise-not n))]
               [else (error 'handle-unary "Tried to invert a non-number")])]
    ['tagof (VStr (get-tag arg))]
    ['length (type-case CVal arg
               [VStr (str) (VNum (string-length str))]
               ;[VList (elts uid) (VNum (length (hash-keys elts)))]
               ;[VDict (elts uid) (VNum (length (hash-keys elts)))]
               ;[VTuple (elts uid) (VNum (length (hash-keys elts)))]
               [VHash (elts uid t) (VNum (length (hash-keys elts)))]
               [else (error 'interp-length "Should only be called on strings, lists, dicts and tuples.")])]
    ['to-bool (if (isTruthy arg) (VTrue) (VFalse))]
    ['to-string (VStr (pretty arg))]
    ['to-float (type-case CVal arg
                 [VFalse () (VNum 0.0)]
                 [VTrue () (VNum 1.0)]
                 [VNum (n) (VNum (+ 0.0 n))]
                 [VStr (s) (error 'interp-to-num "String to Num not implemented yet.")] ;; TODO handle outside...
                 [else (error 'interp-to-num "Should not be called on this type.")])]
    ['to-int (type-case CVal arg
               [VFalse () (VNum 0)]
               [VTrue () (VNum 1)]
               [VNum (n) (VNum n)] ;; TODO figure this out...
               [VStr (s) (error 'interp-to-num "String to Num not implemented yet.")]
               [else (error 'interp-to-num "Should not be called on this type.")])]
    ['to-list (type-case CVal arg
                [VHash (elts uid type) (if (or (isInstanceOf arg (Type "list" (list))) (isInstanceOf arg (Type "tuple" (list))))
                                           (VHash elts (new-uid) (Type "list" (list)))
                                           (error 'interp-to-list "arguments of this type are not supported"))]
                [VStr (s) (VHash (change-string-to-list s) (new-uid) (Type "list" (list)))] ;; string to list
                [else (error 'interp-to-list "Unsupported Type")])]
    ['to-tuple (type-case CVal arg
                [VHash (elts uid type) (if (or (isInstanceOf arg (Type "list" (list))) (isInstanceOf arg (Type "tuple" (list))))
                                           (VHash elts (new-uid) (Type "tuple" (list)))
                                           (error 'interp-to-list "arguments of this type are not supported"))]
                [VStr (s) (VHash (change-string-to-list s) (new-uid) (Type "tuple" (list)))] ;; string to list
                [else (error 'interp-to-list "Unsupported Type")])]
    [else (error prim "handle-unary: Case not handled yet")]))

;; wrapper around unary operations
(define (interp-unary [prim : symbol] [arg : CExp] [env : Env] [store : Store]) : AnswerC
  (type-case AnswerC (interp-env arg env store)
    [ValueA (v s) (ValueA (handle-unary prim v) s)]
    [ExceptionA (v s) (ExceptionA v s)]
    [ReturnA (v s) (ReturnA v s)]))

;;interps every object represented as a hash
(define (interp-CHash [keys : (listof CExp)]
                      [h : (hashof CExp CExp)]
                      [type : VType]
                      [env : Env]
                      [store : Store]) : AnswerC 
  (cond
    [(empty? keys) (ValueA (VHash (hash (list)) (new-uid) type) store)]
    [(cons? keys) 
     (type-case AnswerC (interp-env (first keys) env store)
       [ValueA (v1 s1)
               (type-case (optionof CExp) (hash-ref h (first keys))
                 [none () (error 'interp-CHash "Cannot find key inside hash with this key: something is very wrong")]
                 [some (value) 
                       (type-case AnswerC (interp-env value env s1)
                         [ValueA (v2 s2) (type-case AnswerC (interp-CHash (rest keys) h type env s2)
                                           [ValueA (v3 s3) (ValueA (VHash (hash-set (VHash-elts v3) v1 v2) (VHash-uid v3) type) s3)]
                                           [ExceptionA (v3 s3) (ExceptionA v3 s3)]
                                           [ReturnA (v3 s3) (error 'interp-CHash "Shouldn't see a return here...")])]
                         [ExceptionA (v2 s2) (ExceptionA v2 s2)]
                         [ReturnA (v2 s2) (error 'interp-CHash "This should never be a return.")])])]
       [ExceptionA (v1 s1) (ExceptionA v1 s1)]
       [ReturnA (v1 s1) (error 'interp-CHash "This should never be a return!!")])]))


;; TODO somehow we need to have a function to convert a CId into a string
(define (id-to-string (id : CExp)) : string
  (type-case CExp id
    [CId (s) (symbol->string s)]
    [else (error 'id-to-string "Not an ID")]))

;; -------------------------------------------------------------HAVE TO ADAPT THIS TO INHERITANCE WHEN IT COMES THE TIME-------------------------
;; -------------------------------------------------------------HAVE TO ADAPT THIS TO INHERITANCE WHEN IT COMES THE TIME-------------------------
;; isInstanceOf checks whether 'obj' is of the same type or of one of the base types of 'type'
(define (isInstanceOf [obj : CVal]
                      [type : VType]) : boolean
  (equal? (get-tag obj) (Type-name type)))

;; helper function that interps a CExp (that is supposed to be a VClass) to a type
(define (CExpToType [type : CExp]
                    [env : Env]
                    [store : Store]) : VType
  (VClass-type (lookupStore (lookupVar (CId-x type) env) store)))

;; TODO This needs to be adapted to work with integers and other primitive types as well...

;; hasMatchingException checks whether any of the except clauses deal with the raised object
(define (hasMatchingException [exc : CVal] 
                              [handlers : (listof CExceptionHandler)]
                              [env : Env]
                              [store : Store]) : boolean
  (cond
    [(empty? handlers) false]
    [(cons? handlers) 
     (if (CNone? (CExcHandler-type (first handlers)))
         true
         (if (isInstanceOf exc (CExpToType (CExcHandler-type (first handlers)) env store))
             true
             (hasMatchingException exc (rest handlers) env store)))]))
                      


;; THIS DOES NOT CATCH THE CORRECT EXCEPTION YET!!!!! WE NEED TO IMPLEMENT TYPES BEFORE WE DO THIS. FOR NOW, WE JUST MATCH THE FIRST RESULT
;; THIS ALSO DOES NOT IMPLEMENT THE BINDING OF THE NAME WITH THE TYPE
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(define (interp-handlers [handlers : (listof CExceptionHandler)] [val : CVal] [env : Env] [store : Store]) : AnswerC 
  (cond
    [(empty? handlers) (ExceptionA val store)]
    [(cons? handlers) (interp-env (CExcHandler-body (first handlers)) env store)]))

;; getAttr gets the attribute from an object (VHash) -----------------------------------------------------------throw an exception instead of an error
(define (getAttr [attr : symbol]
                 [obj : CVal]) : CVal
  (type-case CVal obj
    [VHash (elts uid type) (type-case (optionof CVal) (hash-ref elts (VStr (symbol->string attr)))
                             [none () (error 'getAttr "non-existent attribute")]
                             [some (v) v])]
    [VClass (elts type) (type-case (optionof CVal) (hash-ref elts (VStr (symbol->string attr)))
                          [none () (error 'getAttr "non-existent attribute")]
                          [some (v) v])]
    [else (error 'getAttr "tried to get attribute from non-object")]))


;(define-type DefArgHolder
  

;; 
;(define (interp-defargs [defargs : (listof CExp)] [env : Env] [store : Store]) : DefArgHolder
  

(define (interp-func [args : (listof symbol)]
                     [vararg : symbol]
                     [body : CExp] 
                     [vlist : (listof (ScopeType * symbol))]
                     [defargs : (listof CExp)] 
                     [defvals : (listof CVal)]
                     [env : Env]
                     [store : Store]) : AnswerC
  (cond 
    [(empty? defargs) (ValueA (VClosure (newEnvScope env vlist args vararg) 
                                        args 
                                        vararg
                                        body 
                                        (reverse defvals) 
                                        (new-uid)) 
                              store)]
    [else (type-case AnswerC (interp-env (first defargs) env store)
            [ValueA (v s) (interp-func args vararg body vlist (rest defargs) (cons v defvals) env s)]
            [ExceptionA (v s) (ExceptionA v s)]
            [ReturnA (v s) (error 'interp-func "I don't think this should even happen.")])]))

;;creates a hash with all of the positional arguments
(define (group-positional-arguments [ids : (listof symbol)]
                                    [args : (listof CExp)]) : (hashof symbol CExp)
  (cond
    [(or (empty? args) (empty? ids))
     (hash (list))]
    [else
     (hash-set (group-positional-arguments (rest ids)
                                           (rest args))
               (first ids)
               (first args))]))

;;adds the keyword arguments to the hash of arguments, and throws an error if we have one
(define (group-keyword-arguments [ids : (listof symbol)]
                                 [keywargs : (listof (symbol * CExp))]
                                 [argList : (hashof symbol CExp)]) : (hashof symbol CExp)
  (cond
    [(empty? keywargs) argList]
    [else
     (local [(define-values (id exp) (first keywargs))]
       (if (member id ids)
           (if (member id (hash-keys argList))
               (error 'group-keyword-arguments "keyword argument for argument already defined by positional argument: throw TypeError exception")
               (group-keyword-arguments ids 
                                        (rest keywargs)
                                        (hash-set argList id exp)))
           (error 'group-keyword-arguments "non-existing keyword argument: throw a TypeError exception.")))]))

;;creates an ordered list of arguments with CUnbound representing the default arguments in the position of missing arguments.
(define (group-all-arguments [ids : (listof symbol)]
                             [argList : (hashof symbol CExp)]
                             [varargid : symbol]
                             [varargs : (listof CExp)]) : (listof CExp)
  (let [(arguments (foldl (lambda (id lst) (type-case (optionof CExp) (hash-ref argList id)
                                             [none () (append lst (list (CUnbound)))]
                                             [some (v) (append lst (list v))]))
                          (list)
                          ids))]
    (if (not (equal? varargid 'no-vararg))
        (append arguments (list (create-clist varargs)))
        arguments)))

;;checks if the list of arguments has some CUnbound in a position that is not from some default argument
(define (group-check-defaults [ids : (listof symbol)]
                              [nDefArgs : number]
                              [argsList : (listof CExp)]) : boolean
  (if (member (CUnbound) (drop-right argsList nDefArgs))
      false
      true))

;; group-arguments create a list of arguments in the correct order from the list of positional args, keyword args and the number of existing default args.
;; if varargid is different from 'no-vararg, adds a new argument to the list of arguments, that is a list containing the positional arguments in excess
(define (group-arguments [ids : (listof symbol)]
                         [varargid : symbol]
                         [args : (listof CExp)]
                         [keywargs : (listof (symbol * CExp))]
                         [nDefArgs : number]
                         [argList : (hashof symbol CExp)]
                         [varargs : (listof CExp)]) : (listof CExp)
  (cond
    [(and (< (length ids) (length args)) (equal? varargid 'no-vararg))
     (error 'group-arguments "arity mismatch: more arguments than function can handle. Should throw TypeError exception.")]
    [(not (empty? args))
     (group-arguments ids
                      varargid
                      (list)
                      keywargs
                      nDefArgs
                      (group-positional-arguments ids args)
                      (if (< (length ids) (length args))
                          (list-tail args (length ids))
                          (list)))]
    [(not (empty? keywargs))
     (group-arguments ids
                      varargid
                      (list)
                      (list)
                      nDefArgs
                      (group-keyword-arguments ids keywargs argList)
                      varargs)]
    [else
     (if (group-check-defaults ids
                               nDefArgs
                               (drop-right (group-all-arguments ids
                                                                argList
                                                                varargid
                                                                varargs)
                                           1))
         (group-all-arguments ids
                              argList
                              varargid
                              varargs)
         (error 'group-arguments "missing argument: throw TypeError exception"))]))


;; helper functions to create ranges of numbers
;###########################
(define (cnum-range [n : number]) : (listof CExp)
  (map (lambda (x) (CNum x)) (range2 n)))

(define (range2 [n : number]) : (listof number)
  (reverse (range-backwards n)))

(define (range-backwards2 [n : number]) : (listof number)
  (cond
    [(<= n 0) empty]
    [else (cons (- n 1) (range-backwards (- n 1)))]))

(define (create-hash [keys : (listof CExp)]
                     [vals : (listof CExp)]) : (hashof CExp CExp)
  (cond
    [(and (empty? keys) (empty? vals)) (hash (list))]
    [(and (cons? keys) (cons? vals)) (hash-set (create-hash (rest keys) (rest vals)) (first keys) (first vals))]
    [else (error 'create-hash "key and value lists do not match")]))
 
(define (create-clist [exps : (listof CExp)]) : CExp
  (CHash (create-hash (cnum-range (length exps)) exps) (Type "list" (list))))

;; interp-env
(define (interp-env [expr : CExp] 
                    [env : Env] 
                    [store : Store]) : AnswerC
  (type-case CExp expr
    [CNum (n) (ValueA (VNum n) store)]
    [CStr (s) (ValueA (VStr s) store)]
    [CTrue () (ValueA (VTrue) store)]

    [CError (e) (type-case AnswerC (interp-env e env store)
                  [ValueA (v s) (ExceptionA v s)]
                  [ExceptionA (v s) (ExceptionA v s)]
                  [ReturnA (v s) (error 'CError "should not get a Return statement when raising something")])]
            ;(error 'interp (pretty (ValueA-value (interp-env e env store))))] ;; exception
                  
    [CReturn (value) (type-case AnswerC (interp-env value env store)
                       [ValueA (v s) (ReturnA v s)]
                       [ExceptionA (v s) (ExceptionA v s)]
                       [ReturnA (v s) (error 'interp "Return statement inside of Return...")])]
    

    [CId (x) 
         (ValueA (lookupStore (lookupVar x env) store) store)]

    [CLet (id scopeType bind body)
      (type-case AnswerC (interp-env bind env store)
        [ValueA (v s)
                (let ([newLocation (new-loc)])
                  (interp-env body
                              (augmentEnv id (values scopeType newLocation) env)
                              (augmentStore newLocation 
                                            v
                                            s)))]
        [ExceptionA (v s) (ExceptionA v s)]
        [ReturnA (v s) (ReturnA v s)])] ;; This is a bit suspicious...
                         

    [CSeq (e1 e2)
      (type-case AnswerC (interp-env e1 env store)
        [ValueA (v s)
                (interp-env e2 env s)]
        [ExceptionA (v s) (ExceptionA v s)]
        [ReturnA (v s) (ReturnA v s)])]

    [CSet (id value)
          (type-case CExp id
            [CId (id-symbol) (type-case AnswerC (interp-env value env store)
                               [ValueA (v s)
                                       (ValueA v (augmentStore (lookupEnv id-symbol env)
                                                               v
                                                               s))]
                               [ExceptionA (v s) (ExceptionA v s)]
                               [ReturnA (v s) (ReturnA v s)])]
            [else (error 'interp-CSet "For now, CSet only support ids that are symbols")])]
    
    [CApp (func args keywargs)
     (type-case AnswerC (interp-env func env store)
       [ValueA (vf sf)
         (type-case CVal vf
           [VClosure (e a varg b defargs uid)
                     (interp-args-CApp b   
                                       env
                                       e
                                       sf
                                       (if (not (equal? varg 'no-vararg))
                                           (append a (list varg))
                                           a)
                                       (group-arguments a varg args keywargs (length defargs) (hash (list)) (list))
                                       (list)
                                       defargs)]
           ;;TEMPORARY CASE FOR APPLICATION
           [VClass (elts type) (ValueA (VClass elts type) store)]
           
           [else (error 'CApp (string-append "Applied a non-function: " (pretty vf)))])]
       [ExceptionA (v s) (ExceptionA v s)]
       [ReturnA (v s) (ReturnA v s)] ;; or pass???
       )]
    #|
    (type-case CVal (interp-env fun env)
       [VClosure (env argxs body)
         (local [(define argvs (map (lambda (e) (interp-env e env)) arges))]
          (interp-env body (bind-args argxs argvs env)))]
       [else (error 'interp "Not a closure")])]
    |#

    [CFunc (args body vlist defargs vararg) 
           (interp-func args vararg body vlist defargs (list) env store)]
           ;(ValueA (VClosure (newEnvScope env vlist args) args body () (new-uid)) store)] ;; TODO use vlist...

    [CPrim1 (prim arg) (interp-unary prim arg env store)]
    
    [CPrim2 (op e1 e2)
            (case op
              ;;boolops
              ;; These short-circuit, and so need their own system...
              ['or (interp-or e1 e2 env store)]
              ['and (interp-and e1 e2 env store)]
              ['is (interp-is e1 e2 env store)] ;; might want to think about these...
              ['isNot (interp-isNot e1 e2 env store)]
              ['in (interp-in e1 e2 env store)]
  ;            ['isinstance (if (isInstanceOf e1 (Type () (list))) 
  ;                             (ValueA (VTrue) store)
  ;                             (ValueA (VFalse) store))]
              
              [else (interp-binop op e1 e2 env store)])
            ]
    
    [CIf (i t e)
         (type-case AnswerC (interp-env i env store)
           [ValueA (v s)
                   (if (isTruthy v)
                       (interp-env t env s)
                       (interp-env e env s))]
           [ExceptionA (v s) (ExceptionA v s)]
           [ReturnA (v s) (ReturnA v s)])]
    [CNone () (ValueA (VNone) store)]
    [CFalse () (ValueA (VFalse) store)] 
    [CPass () (ValueA (VNone) store)] ;; doing nothing. We need a case for that...
    [CUnbound () (ValueA (VUnbound) store)]
    [CGlobalEnv () 
                (begin
                  (set! globalEnv (createGlobalEnv env))
                  (ValueA (VNone) store))]
    [CAttribute (attr value)
                (type-case AnswerC (interp-env value env store)
                  [ValueA (v s) (type-case CVal v
                                  [VHash (elts uid type) (ValueA (getAttr attr v) s)]
                                  [VClass (elts type) (ValueA (getAttr attr v) s)]
                                  [else (interp-env (CError (CStr "tried to get an attribute from a return expression")) env s)])]
                  [ExceptionA (v s) (ExceptionA v s)]
                  [ReturnA (v s) (error 'CAttribute "should not get an attribute from a return expression")])]
    #|
    [CDict (dict) (type-case AnswerC (interp-dict-insides (hash-keys dict) dict env store)
                    [ValueA (v s) (ValueA v s)]
                    [ExceptionA (v s) (ExceptionA v s)]
                    [ReturnA (v s) (error 'interp "Something is wrong here - shuldn't be a return inside a dict. ")])]
    
    [CList (l) (type-case AnswerC (interp-list-insides (hash-keys l) l env store)
                 [ValueA (v s) (ValueA v s)]
                 [ExceptionA (v s) (ExceptionA v s)]
                 [ReturnA (v s) (error 'interp "Something is wrong here - shuldn't be a return inside a list. ")])]
    [CTuple (elts) (type-case AnswerC (interp-list-insides (hash-keys elts) elts env store)
                 [ValueA (v s) (ValueA v s)]
                 [ExceptionA (v s) (ExceptionA v s)]
                 [ReturnA (v s) (error 'interp "Something is wrong here - shuldn't be a return inside a tuple. ")])]
|#
    [CHash (h type) (interp-CHash (hash-keys h) h type env store)]
    
    ;;This class is just temporary, so that we can pass exception tests
    [CClass (elts type) (ValueA (VClass elts type) store)]
    
    ;; exception handling
    [CTryExcept (body handlers orelse) 
                (type-case AnswerC (interp-env body env store)
                  [ValueA (v s) (interp-env orelse env s)]
                  [ExceptionA (v s) (if (hasMatchingException v handlers env s)
                                        (interp-handlers handlers v env s)
                                        (interp-env orelse env s))]
                  [ReturnA (v s) (ReturnA v s)])]
    [CTryFinally (body finalbody) (type-case AnswerC (interp-env body env store)
                                    [ValueA (v s) (interp-env finalbody env s)]
                                    [ExceptionA (v s) (type-case AnswerC (interp-env finalbody env s)
                                                        [ValueA (v2 s2) (ExceptionA v s2)]
                                                        [ExceptionA (v2 s2) (ExceptionA v2 s2)]
                                                        [ReturnA (v2 s2) (ReturnA v2 s2)])]
                                    [ReturnA (v s) (type-case AnswerC (interp-env finalbody env s)
                                                        [ValueA (v2 s2) (ReturnA v s2)]
                                                        [ExceptionA (v2 s2) (ExceptionA v2 s2)]
                                                        [ReturnA (v2 s2) (ReturnA v2 s2)])])]
    
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
    [ValueA (v s) v]
    [ExceptionA (v s) (error 'exception (pretty v))] ;; really? 
    [ReturnA (v s) (VStr "Error: Return outside of function.")]))


;; basic test cases
;;(interp (CTrue))

(define env (hash (list (values 'a (values (Local) 1)) (values 'b (values (NonLocal) 2)) (values 'c (values (Global) 3)))))
(define h (hash (list (values 'x (values (Local) 1)) (values 'y (values (NonLocal) 2)))))

;(make-new-map 
;   (list (VNum 0) (VNum 1) (VNum 2) (VNum 3))
;   (list (VStr "s") (VStr "p") (VStr "a") (VStr "m")))