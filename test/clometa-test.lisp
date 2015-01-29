(in-package :clometa-test)

(in-root-suite)

(defsuite* test-all)

(define-ometa std
  (char (seq* (bind c (apply anything))
              (->? (characterp c))
              (-> c)))
  ;; why bake character-classes into a language when
  ;; its so expressive that adding them is trivial
  (char-range x y
              (seq* (bind c (apply anything))
                    (->? (and (characterp c)
                              (char<= x c y)))
                    (-> c)))
  (letter (alt* (apply char-range #\a #\z)
                (apply char-range #\A #\Z)))
  (digit (apply char-range #\0 #\9))
  (number (many+ (apply digit)))
  (spaces (many+ (atom #\space))))

(defun char->number (char)
  (case char
    (#\0 0)
    (#\1 1)
    (#\2 2)
    (#\3 3)
    (#\4 4)
    (#\5 5)
    (#\6 6)
    (#\7 7)
    (#\8 8)
    (#\9 9)))

(setf (symbol-function 'char->number)
      (alexandria:compose #'parse-integer #'list->string #'list))

(define-ometa integers (:<< std)                         ;inherit from std
  (int (alt* (seq* (bind n (apply int))
                   (bind d (apply (^ digit)))          ;invoke a `digit' parent rule
                   (-> (+ (* n 10) (char->number d))))
             (seq* (bind d (apply (^ digit)))
                   (-> (char->number d))))))
