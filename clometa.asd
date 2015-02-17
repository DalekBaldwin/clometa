;;;; clometa.asd

(defpackage :clometa-system
  (:use :cl :asdf))
(in-package :clometa-system)

(defsystem :clometa
  :name "clometa"
  :serial t
  :components
  ((:static-file "clometa.asd")
   (:module :src
            :components ((:file "package")
                         (:file "interpreter-utils")
                         (:file "interpreter")
                         (:file "compiler")
                         )
            :serial t))
  :depends-on (:alexandria
               :anaphora
               :optima
               :fare-quasiquote-extras
               :cl-hash-util
               :contextl))

(defsystem :clometa-test
  :name "clometa-test"
  :serial t
  :components
  ((:module :test
            :components ((:file "package")
                         (:file "interpreter-test")
                         (:file "util")
                         (:file "compiler-test"))
            :serial t))
  :depends-on (:clometa :stefil :cl-hash-util))
