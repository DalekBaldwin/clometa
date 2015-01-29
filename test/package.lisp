(in-package :cl-user)

(defpackage :clometa-test
  (:use :cl :clometa :stefil)
  (:export
   #:test-all))

(in-package :clometa-test)

(defparameter *system-directory* clometa::*system-directory*)
