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
  (is (equal (multiple-value-list (gomatch std letter () (list #\a #\1)))
             '(#\a (#\1)))))

(defgrammar simple-binding ()
  (start ()
         (list
          (bind x 1)
          (bind y 2))
         :-> (list x y)))

(deftest test-simple-binding ()
  (is (equal (multiple-value-list
              (gomatch simple-binding start () '((1 2))))
             '((1 2) nil))))

(defgrammar simple-binding-call ()
  (start ()
         (list
          (bind x _)
          (bind y (or 3 (start))))
         :-> (list x y)))

(deftest test-binding-call ()
  (is (equal (multiple-value-list
              (gomatch simple-binding-call start () '((1 (2 3)))))
             '((1 (2 3)) nil))))

(defgrammar simple-apply ()
  (list-of (p)
           (bind first (apply p))
           (bind rest (* (seq #\, (apply p))))
           :-> (cons first rest))
  (one () #\1)
  (start ()
         (list-of #'one)))

(deftest test-simple-apply ()
  (is (equal (multiple-value-list
              (gomatch simple-apply start () (list #\1 #\, #\1 #\, #\1)))
             '((#\1 #\1 #\1) nil))))

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
              (gomatch left-recursion start () (list #\1 #\+ #\2 #\- #\3)))
             '((sub (add #\1 #\2) #\3) nil))))

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
  (is (equal (multiple-value-list
              (gomatch direct-left-recursion start () (list #\1 #\- #\2 #\- #\3)))
             '(((#\1 #\2) #\3) nil))))

(defgrammar empty-1 ()
  (start () (list (end)))
  (end () (~ _)))

(defgrammar empty-2 ()
  (start () (list (list (end))))
  (end () (~ _)))

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

(defgrammar integers (std)
  (int () (or (seq (bind n (int))
                   (bind d (foreign (^ digit)))
                   :-> (+ (* n 10) (char->number d)))
              (seq (bind d (foreign (^ digit)))
                   :-> (char->number d)))))

#+nil
(deftest test-integers ()
  (is (= (gomatch integers int () (list #\5 #\6 #\7);;"567"
                  )
         567)))

(defgrammar token (std)
  (letter ()
          (or #\_
              (foreign (^ letter))))
  (id ()
      (+ (letter)))
  (num ()
       (or (seq (bind pre (foreign (^ number)))
                (atom #\.)
                (bind post (foreign (^ number)))
                :-> `(,@pre #\. ,@post))
           (foreign (^ number)))))
#+nil
(deftest test-token ()
  (is (equal (gomatch token id () "hello_Id")
             '(#\h #\e #\l #\l #\o #\_ #\I #\d)))
  (is (equal (gomatch token number () "57.877")
             '(#\5 #\7 #\. #\8 #\7 #\7))))

(defgrammar flat ()
  (flatten ()
           (list (bind xs (inside)))
           :-> xs)
  (inside ()
          (or
           (seq (list (bind xs (inside)))
                (bind ys (inside))
                :-> (append xs ys))
           (seq (bind x _)
                (bind xs (inside))
                :-> (cons x xs))
           (seq (end)
                :-> nil)))
  (end ()
       (~ _)
       :-> nil))
#+nil
(deftest test-flatten ()
  (is (equal (gomatch flat flatten () '((1 (2 (3 4) (5 6)) (((7))))))
             '(1 2 3 4 5 6 7))))

(defgrammar toks (std)
  (equ ()
       #\=
       :-> (cl-hash-util:hash-create `((kind =) (value "="))))
  (num () (bind n (foreign (^ number)))
       :-> (cl-hash-util:hash-create `((kind num) (value ,(list->string n)))))
  (id ()
      (bind ls (foreign (^ id token)))
      :-> (cl-hash-util:hash-create `((kind id) (value ,(list->string ls)))))
  (scanner ()
           (foreign (^ spaces))
           (or (equ) (num) (id))))

(defgrammar assignments (toks)
  (token (k)
         (bind tok (foreign (^ scanner)))
         :->? (equal (gethash 'kind tok nil) k)
         :-> (gethash 'value tok))
  (assign ()
          (bind a (token 'id))
          (bind b (token '=))
          (bind c (token 'num))
          :-> (concatenate 'string a b c)))
