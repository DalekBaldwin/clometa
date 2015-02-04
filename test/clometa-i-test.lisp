(in-package :clometa.i-test)

(in-root-suite)

(defsuite* test-all)

(defparameter *depth* 0)
(defparameter *introspect* nil)
#+nil
(setf *introspect* t)
#+nil
(setf *introspect* nil)

(defmethod clometa.i::dispatch :around (head exp ;;stream store
                                        )
  (when *introspect*
    (format t "~&(~A> d ~S~%~A ~S~%"
            (with-output-to-string (s)
              (dotimes (i *depth*)
                (princ "-" s)))
            (let ((str (princ-to-string exp)))
              (subseq str 0 (min 40 (length str))))
            
            (mapcar #'cdr clometa.i::*stream*)
             clometa.i::*store*))
  (let ((*depth* (1+ *depth*)))
    (let ((result
           (multiple-value-list
            (call-next-method))))
      (when *introspect*
        (format t "~&<~A~%~A ~S)~%"
                (with-output-to-string (s)
                  (dotimes (i (1- *depth*))
                    (princ "-" s)))
                (first exp)
                result))
      (values-list result))))
#+nil
(defmethod clometa.i::dispatch :around ((head symbol) exp ;;stream store
                                        )
  (when (not (member head
                     '(foreign apply empty seq atom alt many many1 ~ bind -> ->? list)))
    (format t "~&HEAD: ~A EXP: ~A~%" head exp))
  (call-next-method))

(defmethod clometa.i::ometa-eval :around (form)
  (when *introspect*
    (format t "~&(~A> e ~S~%"
            (with-output-to-string (s)
              (dotimes (i *depth*)
                (princ "-" s)))
            form))
  (let ((*depth* (1+ *depth*)))
    (let ((result
           (multiple-value-list
            (call-next-method))))
      (when *introspect*
        (format t "~&<~A~S)~%"
                (with-output-to-string (s)
                  (dotimes (i (1- *depth*))
                    (princ "-" s)))
                result))
      (values-list result))))



(define-ometa std
  (char (seq* (bind c (anything))
              (->? (characterp c))
              (-> c)))
  ;; why bake character-classes into a language when
  ;; its so expressive that adding them is trivial
  (char-range x y
              (seq* (bind c (anything))
                    (->? (and (characterp c)
                              (char<= x c y)))
                    (-> c)))
  (letter (alt* (char-range #\a #\z)
                (char-range #\A #\Z)))
  (digit (char-range #\0 #\9))
  (number (many+ (digit)))
  (spaces (many+ (atom #\space))))

(deftest test-letter ()
  (is (eql  (omatch std letter "a1")
             #\a)))

#+nil
(DESUGAR
         '((START
            (SEQ* (LIST (SEQ* (BIND X (ATOM 1)) (BIND Y (ATOM 2)))) (-> (LIST X Y))))))

(define-ometa simple-binding
  (start (seq* (list
                (seq* (bind x (atom 1))
                      (bind y (atom 2))))
               (-> (list x y)))))

(deftest test-binding ()
  (is (equal  (omatch simple-binding start (list 1 2))
               (list 1 2))))

#+nil
(omatch
 (ometa
  (barf (alt* (barf) (anything))))
 barf
 "5")

#+nil
(omatch
 (ometa
  (barf (alt* (list (seq* (atom 1) (atom 2)))
              (list (many (atom 3))))))
 barf
 (list 3 3 3 3 3))

(define-ometa simple-binding-apply
  (start (seq*
          (list
           (seq*
            (bind x (anything))
            (bind y (alt* (atom 3)
                          (start)))))
          (-> (list x y)))))

(deftest test-binding-apply ()
  (is (equal  (omatch simple-binding-apply start (list 1 (list 2 3)))
               (list 1 (list 2 3)))))

(define-ometa left-recursion
  (start (alt* (seq* (bind x (start))
                     (atom #\-)
                     (bind y (N))
                     (-> (list 'sub x y)))
               (seq* (bind x (start))
                     (atom #\+)
                     (bind y (N))
                     (-> (list 'add x y)))
               (N)))
  (N (alt* (atom #\1)
           (atom #\2)
           (atom #\3))))

(deftest test-left-recursion ()
  (is (equal  (omatch left-recursion start "1+2-3")
               '(sub (add #\1 #\2) #\3))))

(define-ometa direct-left-recursion
  (start (alt* (seq* (bind x (start))
                     (atom #\-)
                     (bind y (N))
                     (-> (list x y)))
               (N)))
  (N (alt* (atom #\1)
           (atom #\2)
           (atom #\3))))

(deftest test-direct-left-recursion ()
  (is (equal  (omatch direct-left-recursion start "1-2-3")
             '((#\1 #\2) #\3))))

(define-ometa empty-1
  (start (list (end)))
  (end   (~ (anything)))
  (derp (anything)))

(define-ometa empty-2
  (Start (list (list (End))))
  (End   (~ (anything))))


(define-ometa empty-3
  (Start (list (list (End))))
  (End   (~ (anything))))



(deftest test-empty ()
  #+nil
  (is (omatch empty-1 end '()))
  (is (eql clometa.i::*failure-value* (omatch empty-2 start '())))
  #+nil
  (is  (omatch empty-3 start '(()))))

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
#+nil
(DESUGAR
         '((INT
            (ALT*
             (SEQ* (BIND N (INT)) (BIND D (apply (^ DIGIT)))
              (-> (+ (* N 10) (CHAR->NUMBER D))))
             (SEQ* (BIND D (foreign (^ DIGIT))) (-> (CHAR->NUMBER D))))))
         (LIST STD))

(define-ometa integers (:<< std)                         ;inherit from std
  (int (alt* (seq* (bind n (int))
                   (bind d (foreign (^ digit)))          ;invoke a `digit' parent rule
                   (-> (+ (* n 10) (char->number d))))
             (seq* (bind d (foreign (^ digit)))
                   (-> (char->number d))))))

(deftest test-integers ()
  (is (= (omatch integers int "567")
         567)))

(omatch
 (ometa
  (derp
   (list (seq* (anything) (many (atom 'barf))))))
 derp
 (list 'narf 'barf))

#+nil
(omatch
 (ometa (:<< std)
  (zchar-range x y
              (seq* (bind c (anything))
                    (->? (and (characterp c)
                              (char<= x c y)))
                    (-> c)))
  (zdigit (zchar-range #\0 #\9))
  (int (alt* (seq* (bind n (int))
                   (bind d (zdigit))
                   (-> (+ (* n 10) (char->number d))))
             (seq* (bind x (foreign (^ digit)))
                   (-> (char->number x))))))
 int
 "5")

#+nil
(omatch integers int "5")

(define-ometa token (:<< std)
  (letter (alt* (atom #\_)              ; accept underscore
                (foreign (^ letter))))    ; invoke parent rule
  (id (many+ (letter)))
  (number (alt* (seq* (bind pre (foreign (^ number)))
                      (atom #\.)
                      (bind post (foreign (^ number)))
                      (-> `(,@pre #\. ,@post)))
                (foreign (^ number)))))

(deftest test-token ()
  (is (equal  (omatch token id "hello_Id")
             '(#\h #\e #\l #\l #\o #\_ #\I #\d)))
  (is (equal  (omatch token number "57.877")
             '(#\5 #\7 #\. #\8 #\7 #\7))))

(define-ometa flat
  (flatten (seq*
            (list (bind xs (inside)))
            (-> xs)))
  (inside  (alt*
            (seq* (list (bind xs (inside)))
                  (bind ys (inside))
                  (-> (append xs ys)))
            (seq* (bind x (anything))
                  (bind xs (inside))
                  (-> (cons x xs)))
            (seq* (end)
                  (-> nil)
                  )))
  (end  (seq*
         (~ (anything))
         (-> nil)
         )))

(deftest test-flatten ()
  (is (equal  (omatch flat flatten '(1 (2 (3 4) (5 6)) (((7)))))
             '(1 2 3 4 5 6 7))))

(define-ometa toks (:<< std)
  (eq  (seq* (atom #\=)
             (-> (cl-hash-util:hash-create `((kind =) (value "="))))))
  (num (seq* (bind n (foreign (^ number)))
             (-> (cl-hash-util:hash-create `((kind num) (value ,(list->string n)))))))
  ;; not just rules inherited from `std'
  ;; let's invoke one from `token'
  (id  (seq* (bind ls (foreign (^ id token)))
             (-> (cl-hash-util:hash-create `((kind id) (value ,(list->string ls)))))))
  (scanner (seq* (foreign (^ spaces))
                 (alt* (eq)
                       (num)
                       (id)))))

(define-ometa assignments (:<< toks)
  (token k (seq* (bind tok (foreign (^ scanner)))
                 (->? (equal (gethash 'kind tok nil) k))
                 (-> (gethash 'value tok))))
  (assign (seq* (bind a (token 'id))
                (bind b (token '=))
                (bind c (token 'num))
                (-> (concatenate 'string a b c)))))
#+nil
(omatch assignments assign " my_var    = 56")
#+nil
(first
 (omatch assignments assign "my_var = 56"))

(define-ometa lambda-list
  (symbol (seq* (bind s (anything))
                (->? (symbolp s))
                (-> s)))
  (llkey (seq* (bind s (symbol))
               (->? (member s '(:&optional :&rest :&key :&allow-other-keys :&aux)))
               (-> s)
               ))
  (required (seq* (~ (llkey))

                  (bind j (symbol))
                  (-> (list :required j))
                  ))

  (optional (seq* 
             (~ (llkey))
             (alt* (bind name (symbol))
                   (list (seq* (bind name (symbol))
                               (bind init-form (anything))
                               (bind supplied-p (symbol)))
                         
                  ;;; same problem as with optima:
                  ;;; how to express extending horizon of optional elements?

                         ))
             (-> (list :optional name ;;init-form supplied-p
                       ))
             ))
  (key (seq*
        (~ (llkey))
        (alt* (bind name (symbol))
              (list (seq* (bind name (symbol))
                          (bind init-form (anything))
                          (bind supplied-p (symbol)))))
        (-> (list :key name))))
  (aux (seq*
        (~ (llkey))
        (alt* (bind name (symbol))
              (list (seq* (bind name (symbol))
                          (bind init-form (anything)))))))
  (start  (seq* (list (seq* (bind requireds (many (seq* (required))))
                            (bind optionals
                              (many (seq* (atom :&optional)
                                          (many (seq* (optional))))))
                            (bind rest
                              (many1
                               (seq*
                                (atom :&rest)
                                (seq* (~ (llkey))
                                      (symbol)))))
                            (bind keys
                              (many  (seq* (atom :&key)
                                           (many (seq* (key)))
                                           (many (atom :&allow-other-keys))
                                           )))
                            (bind auxes
                              (many (seq*
                                     (atom :&aux)
                                     (bind auxes (many (seq* (aux)))))))))
                (-> (list requireds optionals rest keys auxes)))))
#+nil
(progn
  (format t "~%#########~%")
  (first
   (omatch lambda-list start '(x :&optional y  :&rest rest :&key f :&allow-other-keys :&aux (barf narf)
                               ))))

#+nil
(setf *introspect* nil)


(defparameter *lambda-list-grammar-clhs*
  "lambda-list::= (var* 
                [&optional {var | (var [init-form [supplied-p-parameter]])}*] 
                [&rest var] 
                [&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}* [&allow-other-keys]] 
                [&aux {var | (var [init-form])}*]) ")

(define-ometa clhs
  (whitespace (many+ (seq* (bind char (anything))
                           (alt*
                            (->? (eql char #\Space))
                            (->? (eql char #\Tab))
                            (->? (eql char #\Newline)))
                           (-> (list :derp char)))))
  (space (atom #\Space))
  (opt (seq* (atom #\[)
             (bind stuff
               (whitespace))
             (atom #\])
             (-> (list stuff))))
  (symbol-matcher (many+ (anything))))



#+nil
(progn
  (format t "~&#####~%")
  (omatch clhs opt "[  ]"))


#+nil
(first
 (omatch clhs opt "[ ]"))

(clometa.i::desugar '((opt (seq*  (bind lbracket (anything))
                            (->? (eql lbracket #\[))
                            (many+ (anything))
                            (bind rbracket (anything))
              (->? (eql rbracket #\]))))))

#+nil
(progn
  (format t "~&#####~%")
  (first
   (omatch
    (ometa
     (delimit x y r (seq*
                     (bind a (anything))
                     (-> (eql a x))
                     (many (seq* (bind j (apply r))
                                 (->? (not (eql j y)))))
                     (bind b (anything))
                     (-> (eql b y))))
     (a? (atom #\a))
     (derp (delimit #\[ #\] 'a?)))
    derp
    "[]")))

#+nil
(omatch
 (ometa
  (derp (seq* (~ (atom #\b))
              (many+ (anything)))))
 derp
 "aa")

#+nil
(omatch (ometa
         (list-of p (seq* (apply p)
                          (atom #\,)
                          (apply p)))
         (thing (atom #\a))
         (derp (list-of thing)))
        derp "a,a")
#+nil
(setf *introspect* t)
