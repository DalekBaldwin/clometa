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
