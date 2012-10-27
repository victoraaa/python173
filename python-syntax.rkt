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
  [PyCompare (left : PyExpr) (ops : (listof symbol)) (comparators : (listof PyExpr))]
  [PyPass]
  [PyLambda (args : (listof symbol)) (body : PyExpr)]
  
  [Py-NotExist (a : number)] ;;THIS IS HERE ONLY SO THAT python-desugar won't complain about having completed all of the expressions
  )

