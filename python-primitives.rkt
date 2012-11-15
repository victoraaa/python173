#lang plai-typed

(require "python-core-syntax.rkt")

#|

Since there may end up being a large number of primitives that you
implement for python, here is a suggested factoring into a separate
file.  You can add new primitives here by adding new symbols to the
dispatch.  You might also choose to add more than single-arity
primitives here.

|#

(require (typed-in racket/base [display : (string -> void)]))

(define (pretty arg) : string
  (type-case CVal arg
    [VNum (n) (to-string n)]
    [VStr (s) s]
    [VTrue () "True"]
    [VClosure (env args body uid) (error 'prim "Can't print closures yet")]
    ;;Non-TA code:
    [VNone () "void"]
    [VFalse () "False"]
    [VPass () ""]
    [VUnbound () "Unbound"]
    ;[VDict (h uid) "VDict - for now. We should make this recursively later"]
    ;[VList (h uid) "VList - for now. We should make this recursively later"]
    ;[VTuple (h uid) "VTuple - for now. We'll make it recursive later. "]
    [VHash (h uid type) (Type-name type)]
    ))
  

(define (print arg)
  (display (pretty arg)))

(define (python-prim1 op arg)
  (case op
    [(print) (begin (print arg) arg)]))

