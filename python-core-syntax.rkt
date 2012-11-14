#lang plai-typed

#|

This is the core language; it is just borrowing a few things from 
ParselTongue.

|#

(define-type CExp
  [CNum (n : number)]
  [CStr (s : string)]
  [CTrue]
  [CSeq (e1 : CExp) (e2 : CExp)]
  [CError (e1 : CExp)]
  [CIf (test : CExp) (then : CExp) (else : CExp)]
  [CId (x : symbol)]
  [CLet (id : symbol) (scopeType : ScopeType) (bind : CExp) (body : CExp)]
  [CApp (fun : CExp) (args : (listof CExp))]
  [CFunc (args : (listof symbol)) (body : CExp) (vlist : (listof (ScopeType * symbol)))]
  [CPrim1 (prim : symbol) (arg : CExp)]
  [CPrim2 (op : symbol) (e1 : CExp) (e2 : CExp)]
  [CFalse]
  [CNone]
  [CPass]
  [CReturn (value : CExp)]
  [CSet (id : CExp) (value : CExp)]
  
  ;[CBind (bind : (ScopeType * symbol))] ;;puts an identifier in the environment but does nothing in the store.
  [CUnbound]
  [CGlobalEnv]
  
  ;; lists and dicts become hashes
  [CList (elts : (hashof CExp CExp))]
  [CDict (elts : (hashof CExp CExp))]
  [CTuple (elts : (hashof CExp CExp))]
  
  ;; Exception handling types
  [CTryExcept (body : CExp) (handlers : (listof CExceptionHandler))]
  [CTryFinally (body : CExp) (finalbody : CExp)]
 ; [CExceptionHandler (name : symbol) (type : CExp) (body : CExp)]
  
  [C-NotExist (a : number)] ;;THIS IS HERE ONLY SO THAT python-interp won't complain about having completed all of the expressions
  )

(define-type CExceptionHandler
  [CExcHandler (name : symbol) (type : CExp) (body : CExp)])

(define-type CVal
  [VNum (n : number)]
  [VStr (s : string)]
  [VTrue]
  [VClosure (env : Env) (args : (listof symbol)) (body : CExp) (uid : Uid)]
  ;;I ADDED;;
  [VNone]
  [VFalse]
  [VPass]
  [VUnbound]
  [VList (elts : (hashof CVal CVal)) (uid : Uid)] ;; lists must be keyed byintegers, though...
  [VDict (elts : (hashof CVal CVal)) (uid : Uid)]
  [VTuple (elts : (hashof CVal CVal)) (uid : Uid)]
  )

(define-type-alias Location number)
(define-type ScopeType
  [Local]
  [NonLocal]
  [Global])

(define-type-alias Uid number)

(define-type-alias SLTuple (ScopeType * number))
(define-type-alias Env (hashof symbol SLTuple))
(define-type-alias Store (hashof Location CVal))

(define-type AnswerC
  [ValueA (value : CVal) (store : Store)]
  [ExceptionA (value : CVal) (store : Store)]
  [ReturnA (value : CVal) (store : Store)]
 ; [BreakA (value : CVal) (store : Store)]
 ; [ContinueA (store : Store)]
  )
