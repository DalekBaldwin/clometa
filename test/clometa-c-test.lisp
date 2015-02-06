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
          (bind x 1)
          (bind y 2))
         :-> (list x y)))

(deftest test-simple-binding ()
  (is (equal (multiple-value-list
              (omatch simple-binding start () '((1 2))))
             '((1 2) nil))))

(defgrammar simple-binding-apply ()
  (start ()
         (list
          (bind x _)
          (bind y (or 3
                      (start))))
         :-> (list x y)))

(deftest test-binding-apply ()
  (is (equal (multiple-value-list
              (omatch simple-binding-apply start () '((1 (2 3)))))
             '((1 (2 3)) nil))))

(defgrammar left-recursion ()
  (start ()
         (or
          (seq (bind x (start))
               #\-
               (bind y (n))
               :-> (list 'sub x y))
          (seq (bind x (start))
               #\+
               (bind y (n))
               :-> (list 'add x y))
          (n)))
  (n ()
     (or #\1 #\2 #\3)))

(deftest test-left-recursion ()
  (is (equal (multiple-value-list
              (omatch left-recursion start () (list #\1 #\+ #\2 #\- #\3))
              '((sub (add #\1 #\2) #\3) nil)))))

(defgrammar direct-left-recursion ()
  (start ()
         (or (seq (bind x (start))
                  #\-
                  (bind y (n))
                  :-> (list x y))
               (n)))
  (N ()
     (or #\1 #\2 #\3)))

(deftest test-direct-left-recursion ()
  (is (equal (omatch direct-left-recursion start () (list #\1 #\- #\2 #\- #\3))
             '((#\1 #\2) #\3))))
