#lang plai-typed

(define-type PyExpr
  [PySeq (es : (listof PyExpr))]
  [PyNum (n : number)]
  [PyId (x : symbol)]
  [PyApp (fun : PyExpr) (args : (listof PyExpr))]
  ;;Made by me:
  [PyStr (s : string)]
  [PyIf (test : PyExpr) (then : (listof PyExpr)) (orelse : (listof PyExpr))]
  [PyBoolop (boolop : symbol) (values : (listof PyExpr))]
  [PyUnaryOp (op : symbol) (arg : PyExpr)]
  [PyBinOp (op : symbol) (left : PyExpr) (right : PyExpr)]
  [PyCompare (left : PyExpr) (ops : (listof symbol)) (comparators : (listof PyExpr))]
  [PyPass]
  [PyNone]
  [PyLambda (args : (listof symbol)) (body : PyExpr)]
  [PyRaise (exc : PyExpr)] ;(cause : PyExpr)]
  [PyGlobal (ids : (listof symbol))]
  [PyNonlocal (ids : (listof symbol))]
  
  [PyAssign (targets : (listof PyExpr)) (value : PyExpr)]
  [PyAugAssign (target : PyExpr) (op : symbol) (value : PyExpr)]
  
  [PySet (lhs : PyExpr) (value : PyExpr)]
  [PyModule (program : PyExpr)]
  [PyGlobalEnv]
  
  [PyDef (name : symbol) (args : (listof symbol)) (body : PyExpr)] ;; deffun
  [PyReturn (value : PyExpr)] ;; return
  
  
  ;; Lists, dict, etc
  [PyList (elts : (listof PyExpr))]
  [PyDict (keys : (listof PyExpr)) (values : (listof PyExpr))]
  [PyTuple (elts : (listof PyExpr))]
  
  ;; Exceptions
  [PyTryExcept (body : PyExpr) (handlers : (listof PyExceptHandler)) (orelse : PyExpr)]
  [PyTryFinally (body : PyExpr) (finalbody : PyExpr)]
 ; [PyExceptHandler (name : symbol) (type : PyExpr) (body : (listof PyExpr))]
  
  [Py-NotExist] ;;THIS IS HERE ONLY SO THAT python-desugar won't complain about having completed all of the expressions
  )

(define-type PyExceptHandler
  [PyExcHandler (name : symbol) (type : PyExpr) (body : PyExpr)])