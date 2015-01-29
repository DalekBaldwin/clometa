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
                         (:file "clometa-i"))
            :serial t))
  :depends-on (:alexandria :anaphora :optima :fare-quasiquote-extras))

(defsystem :clometa-test
  :name "clometa-test"
  :serial t
  :components
  ((:module :test
            :components ((:file "package")
                         (:file "clometa-test" :depends-on ("package")))))
  :depends-on (:clometa :stefil))
