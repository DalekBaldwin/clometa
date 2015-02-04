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
                         (:file "util-i")
                         (:file "clometa-i")
                         (:file "clometa-c")
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
                         (:file "clometa-i-test")
                         (:file "clometa-c-test"))
            :serial t
            ))
  :depends-on (:clometa :stefil :cl-hash-util))
