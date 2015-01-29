(in-package :cl-user)

(defpackage :clometa
  (:use :cl :alexandria :anaphora :optima)
  (:export
   #:interp
   #:desugar
   #:desugar-e
   #:ometa
   #:define-ometa
   #:omatch
   
   #:<<
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
