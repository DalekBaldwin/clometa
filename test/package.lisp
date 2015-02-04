(in-package :cl-user)

(defpackage :clometa.i-test
  (:use :cl :clometa.i :stefil :cl-hash-util)
  (:export
   #:test-all))

(defpackage :clometa.c-test
  (:use :cl :clometa.c :stefil :cl-hash-util)
  (:export
   #:test-all))
