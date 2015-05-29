(in-package :clometa.c-test)

(in-root-suite)

(defsuite* test-all)

(defun string->list (string)
  (map 'list #'identity string))

(defun list->string (list)
  (make-array (list (length list))
              :element-type 'character
              :initial-contents list
              :adjustable nil
              :fill-pointer nil
              :displaced-to nil))

(defgrammar std ()
  (char-range (x y)
              (bind c _)
              :->? (and (characterp c)
                        (char<= x c y))
              :-> c)
  (letter () (or
              (char-range #\A #\Z)
              (char-range #\a #\z)))
  (digit () (char-range #\0 #\9))
  (num () (+ (digit)))
  (spaces () (+ #\space)))

(deftest test-letter ()
  (is (equal (multiple-value-list (omatch std letter () (string->list "a1")))
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

(defgrammar simple-binding-call ()
  (start ()
         (list
          (bind x _)
          (bind y (or 3 (start))))
         :-> (list x y)))

(deftest test-binding-call ()
  (is (equal (multiple-value-list
              (omatch simple-binding-call start () '((1 (2 3)))))
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
              (omatch simple-apply start () (string->list "1,1,1")))
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
              (omatch left-recursion start () (string->list "1+2-3")))
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
              (omatch direct-left-recursion start () (string->list "1-2-3")))
             '(((#\1 #\2) #\3) nil))))

(defgrammar indirect-left-recursion ()
  (x ()
     (expr))
  (expr ()
        (or (seq
             (bind butlast (x))
             #\,
             (bind last #\1)
             :-> (append (if (atom butlast)
                             (list butlast)
                             butlast)
                         (list last)))
            #\1)))

(deftest test-indirect-left-recursion ()
  (is (equal (multiple-value-list
              (omatch indirect-left-recursion expr () (string->list "1,1,1,1")))
             '((#\1 #\1 #\1 #\1) nil))))

#+nil
(defgrammar factorial ()
  (fact ((n 0))
        :-> 1)
  (fact (n)
        (bind m (fact (1- n)))
        :-> (* n m)))
#+nil
(omatch factorial fact (0) nil)

#+nil
(defgrammar arg-pat-match ()
  (char-range ((x char) (y char))
              ...))



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
                   (bind d (digit))
                   :-> (+ (* n 10) (char->number d)))
              (seq (bind d (digit))
                   :-> (char->number d)))))

(deftest test-integers ()
  (is (= (omatch integers int () (string->list "567"))
         567)))

(defgrammar token (std)
  (letter ()
          (or #\_
              (next-rule)))
  (id ()
      (+ (letter)))
  (num ()
       (or (seq (bind pre (next-rule))
                #\.
                (bind post (next-rule))
                :-> `(,@pre #\. ,@post))
           (next-rule))))

(deftest test-token ()
  (is (equal (omatch token id () (string->list "hello_Id"))
             '(#\h #\e #\l #\l #\o #\_ #\I #\d)))
  (is (equal (omatch token num () (string->list "57.877"))
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

(deftest test-flatten ()
  (is (equal (omatch flat flatten () '((1 (2 (3 4) (5 6)) (((7))))))
             '(1 2 3 4 5 6 7))))

(defgrammar toks (std)
  (equ ()
       #\=
       :-> (cl-hash-util:hash-create `((kind =) (value "="))))
  (num () (bind n (next-rule))
       :-> (cl-hash-util:hash-create `((kind num) (value ,(list->string n)))))
  (id ()
      (bind ls (foreign token id))
      :-> (cl-hash-util:hash-create `((kind id) (value ,(list->string ls)))))
  (scanner ()
           (spaces)
           (or (equ) (num) (id))))

(defgrammar assignments (toks)
  (token (k)
         (bind tok (scanner))
         :->? (equal (gethash 'kind tok nil) k)
         :-> (gethash 'value tok))
  (assign ()
          (bind a (token 'id))
          (bind b (token '=))
          (bind c (token 'num))
          :-> (concatenate 'string a b c)))

(deftest test-assignments ()
  (is (equal
       (omatch assignments assign () (string->list " my_var    = 56"))
       "my_var=56")))

#+nil
(defgrammar grammar (supergrammar)
  (rule (arg1 arg2)
        (~ 'something) ;; negation - does not consume input
        (or
         (seq 'something-else
              (bind x _)) ;; 
         ()
         
         )
        )
  (rule (arg1 arg2)
        (or (bind x (rule1))
            (bind y (rule2)))
        :->? (test-predicate x y)
        :-> (semantic-action x y)))

(defgrammar yacc-is-dead-russ-cox ()
  (start ()
         (bind stuff (recur))
         (~ _)
         :-> stuff)
  (recur ()
         (or (seq (bind left (recur))
                  '+
                  (bind right (recur))
                  :-> 
                  (list left '+ right))
             'n)))

#-ecl ;; I think this blows ECL's stack
(deftest test-russ-cox ()
  (is (not (eql clometa.c::failure-value
                (omatch yacc-is-dead-russ-cox start ()
                         '(n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n)))))
  (is (eql clometa.c::failure-value
                (omatch yacc-is-dead-russ-cox start ()
                         '(n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + n + + n)))))

(defgrammar list<<-grammar ()
  (thing ()
         (bind x _)
         :->? (integerp x)
         :-> (1+ x))
  (start ()
         (list<< (thing) (thing) (thing))))

(deftest test-list<< ()
  (is (equal
       (omatch list<<-grammar start () (list (list 1 2 3)))
       (list 2 3 4))))

(defgrammar cons-grammar ()
  (thing () _)
  (start ()
         (cons (thing) (thing))))

(deftest test-cons ()
  (is (equal
       (omatch cons-grammar start () (list (cons 1 2)))
       (cons 1 2))))
