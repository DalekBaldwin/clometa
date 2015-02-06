(in-package :cl-user)

(defpackage :clometa.i
  (:use :cl :alexandria :anaphora :optima)
  (:export
   #:interp
   #:desugar
   #:desugar-e
   #:ometa
   #:define-ometa
   #:omatch
   
   #:anything
   
   #:list->string
   
   #:<<
   #:^
   
   #:foreign
   #:empty
   #:seq
   #:seq*
   #:alt
   #:alt*
   #:many
   #:many1
   #:many+
   #:~
   #:bind
   #:->
   #:->?))

(defpackage :clometa.c
  (:use :cl :contextl :optima :alexandria :anaphora)
  (:export
   #:defgrammar
   #:defrule
   #:omatch
   
   #:_
   #:~
   #:bind
   #:seq))

(add-package-local-nickname :i :clometa.i :clometa.c)
