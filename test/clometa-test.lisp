(in-package :clometa.i-test)

(in-root-suite)

(defsuite* test-all)

(defparameter *depth* 0)
(defparameter *introspect* nil)

(defmethod clometa.i::dispatch :around (head exp stream store)
  (when *introspect*
    (format t "~&~A>~S~%"
            (with-output-to-string (s)
              (dotimes (i *depth*)
                (princ "-" s)))
            exp))
  (let ((*depth* (1+ *depth*)))
    (let ((result
           (call-next-method)))
      (when *introspect*
        (format t "~&<~A~S~%"
                (with-output-to-string (s)
                  (dotimes (i (1- *depth*))
                    (princ "-" s)))
                result))
      result)))

(defmethod clometa.i::ometa-eval :around (form)
  (when *introspect*
    (format t "~&~A>~S~%"
            (with-output-to-string (s)
              (dotimes (i *depth*)
                (princ "-" s)))
            form))
  (let ((*depth* (1+ *depth*)))
    (let ((result
           (call-next-method)))
      (when *introspect*
        (format t "~&<~A~S~%"
                (with-output-to-string (s)
                  (dotimes (i (1- *depth*))
                    (princ "-" s)))
                result))
      result)))

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

(deftest test-letter ()
    (is (eql (first (omatch std letter "a1")) #\a)))

(define-ometa simple-binding
  (start (seq* (list
                (seq* (bind x (atom 1))
                      (bind y (atom 2))))
               (-> (list x y)))))

(deftest test-binding ()
    (is (equal (first (omatch simple-binding start (list 1 2)))
               (list 1 2))))

(define-ometa simple-binding-apply
  (start (seq*
          (list
           (seq*
            (bind x (apply anything))
            (bind y (alt* (atom 3)
                          (apply start)))))
          (-> (list x y)))))

(deftest test-binding-apply ()
    (is (equal (first (omatch simple-binding-apply start (list 1 (list 2 3))))
               (list 1 (list 2 3)))))

(define-ometa left-recursion
  (start (alt* (seq* (bind x (apply start))
                     (atom #\-)
                     (bind y (apply N))
                     (-> (list 'sub x y)))
               (seq* (bind x (apply start))
                     (atom #\+)
                     (bind y (apply N))
                     (-> (list 'add x y)))
               (apply N)))
  (N (alt* (atom #\1)
           (atom #\2)
           (atom #\3))))

(deftest test-left-recursion ()
    (is (equal (first (omatch left-recursion start "1+2-3"))
               '(sub (add #\1 #\2) #\3))))

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

(define-ometa integers (:<< std)                         ;inherit from std
  (int (alt* (seq* (bind n (apply int))
                   (bind d (apply (^ digit)))          ;invoke a `digit' parent rule
                   (-> (+ (* n 10) (char->number d))))
             (seq* (bind d (apply (^ digit)))
                   (-> (char->number d))))))

(deftest test-integers ()
  (is (= (first (omatch integers int "567"))
         567)))

(define-ometa token (:<< std)
  (letter (alt* (atom #\_)              ; accept underscore
                (apply (^ letter))))    ; invoke parent rule
  (id (many+ (apply letter)))
  (number (alt* (seq* (bind pre (apply (^ number)))
                      (atom #\.)
                      (bind post (apply (^ number)))
                      (-> `(,@pre #\. ,@post)))
                (apply (^ number)))))

(deftest test-token ()
  (is (equal (first (omatch token id "hello_Id"))
             '(#\h #\e #\l #\l #\o #\_ #\I #\d)))
  (is (equal (first (omatch token number "57.877"))
             '(#\5 #\7 #\. #\8 #\7 #\7))))

(define-ometa flat
  (flatten (seq*
            (list (bind xs (apply inside)))
            (-> xs)))
  (inside  (alt*
            (seq* (list (bind xs (apply inside)))
                  (bind ys (apply inside))
                  (-> (append xs ys)))
            (seq* (bind x (apply anything))
                  (bind xs (apply inside))
                  (-> (cons x xs)))
            (seq* (apply end)
                  (-> nil)
                  )))
  (end  (seq*
         (~ (apply anything))
         (-> nil)
         )))

(deftest test-flatten ()
  (is (equal (first (omatch flat flatten '(1 (2 (3 4) (5 6)) (((7))))))
             '(1 2 3 4 5 6 7))))

#+nil
(define-ometa toks (:<< std)
  (eq  (seq* (atom #\=)
             (-> (cl-hash-util:hash-create `((kind . =) (value . "="))))))
  (num (seq* (bind n (apply (^ number)))
             (-> (make-immutable-hash `((kind . num) (value . ,(list->string n)))))))
  ;; not just rules inherited from `std'
  ;; let's invoke one from `token'
  (id  (seq* (bind ls (apply (^ id token)))
             (-> (make-immutable-hash `((kind . id) (value . ,(list->string ls)))))))
  (scanner (seq* (apply (^ spaces))
                 (alt* (apply eq)
                       (apply num)
                       (apply id)))))

#+nil
(define-ometa assignments (:<< toks)
  (token k (seq* (bind t (apply (^ scanner)))
                 (->? (equal (gethash 'kind t nil) k))
                 (-> (gethash 'value t))))
  (assign (seq* (bind a (apply token 'id))
                (bind b (apply token '=))
                (bind c (apply token 'num))
                (-> (concatenate 'string a b c)))))
#+nil
(omatch assignments assign " my_var    = 56")
