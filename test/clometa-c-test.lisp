(in-package :clometa.c-test)

(in-root-suite)

(defsuite* test-all)

(defgrammar std ()
  (char-range (x y)
              (bind c _)
              :->? (and (characterp c)
                        (char<= x c y))
              :-> c)
  (letter () (or (char-range #\a #\z)
              (char-range #\A #\Z)))
  (digit () (char-range #\0 #\9))
  (num () (+ (digit)))
  (spaces () (+ #\space)))

(deftest test-letter ()
  (is (equal (multiple-value-list (omatch std letter () (list #\a #\1)))
             '(#\a (#\1)))))

(defgrammar simple-binding ()
  (start ()
         (list
          (bind x (atom 1))
          (bind y (atom 2)))
         (-> (list x y))))
