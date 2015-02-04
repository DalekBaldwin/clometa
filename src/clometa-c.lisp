(in-package :clometa.c)
(named-readtables:in-readtable :fare-quasiquote)

;;http://www.tinlizzie.org/ometa-js/#OMeta_Compiler

(defparameter *stream* nil)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-symbol-macro failure-value (load-time-value *failure-value*)))
(defvar *failure-value* (gensym "=FAILURE="))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-symbol-macro empty-value (load-time-value *empty-value*)))
(defvar *empty-value* (gensym "=EMPTY="))

(define-condition match-failure () ())

(defpackage clometa-memos)

(defun memo-for (symbol)
  (find-symbol (symbol-name symbol) :clometa-memos))

(defmacro defrule (name grammar (&rest args) &body body)
  `(define-layered-method ,name
     :in ,grammar ,args
     ,(let ((ast (i:omatch ometa-grammar start body)))
           (i:omatch ast->code start ast))))

(defmacro defgrammar (grammar (&optional supergrammar) &rest rules)
  `(progn
     (deflayer ,grammar ,(when supergrammar (list supergrammar)))
     ,@(loop for rule in rules
          collect
            (destructuring-bind (name (&rest args) &body body)
                rule
              (declare (ignore body))
              `(define-layered-function ,name (,@args)))
          collect
            (destructuring-bind (name (&rest args) &body body)
                rule
              `(defrule ,name ,grammar (,@args) ,@body))
          collect
            `(defparameter ,(intern (symbol-name (first rule)) :clometa-memos)
              (make-hash-table :test 'eql)))))




(defclass ometa-clause ()
  ((bindings
    :accessor bindings
    :initarg :bindings
    :initform nil
    )
   (form
    :accessor form
    :initarg :form)
   (code
    :accessor code
    :initarg :code
    :initform nil)))

(defclass or-clause (ometa-clause)
  ((subclauses
    :accessor subclauses
    :initarg :subclauses)))

(defclass many-clause (ometa-clause)
  ((subclause
    :accessor subclause
    :initarg :subclause)))

(defclass plus-clause (ometa-clause)
  ((subclause
    :accessor subclause
    :initarg :subclause)))

(defclass neg-clause (ometa-clause)
  ((subclause
    :accessor subclause
    :initarg :subclause)))

(defclass bind-clause (ometa-clause)
  ((var
    :accessor var
    :initarg :var)
   (subclause
    :accessor subclause
    :initarg :subclause)))

(defclass ->-clause (ometa-clause)
  ((subclause
    :accessor subclause
    :initarg :subclause)))

(defclass ->?-clause (ometa-clause)
  ((subclause
    :accessor subclause
    :initarg :subclause)))

(defclass list-clause (ometa-clause)
  ((subclause
    :accessor subclause
    :initarg :subclause)))

(defclass seq-clause (ometa-clause)
  ((subclauses
    :accessor subclauses
    :initarg :subclauses)))

(defclass application-clause (ometa-clause)
  ((rule
    :accessor rule
    :initarg :rule)
   (args
    :accessor args
    :initarg :args)))

(defclass atomic-clause (ometa-clause)
  ((thing
    :accessor thing
    :initarg :thing)))

(defclass anything-clause (ometa-clause)
  ())



(i:define-ometa ometa-grammar
  (symbol (i:seq* (i:bind s (i:anything))
                  (i:->? (symbolp s))
                  (i:-> s)))
  (satisfies pred (i:seq* (i:bind s (i:anything))
                          (i:->? (funcall pred s))
                          (i:-> s)
                          ))
  (start (i:seq* (list (i:bind derp (i:many (real-clause))))
                 (i:->  derp)))
  (real-clause
   (i:alt*
    (@or)
    (@many)
    (plus)
    (@neg)
    (@bind)
    (@->)
    (@->?)
    (@list)
    (@seq)
    (@anything)
    (quotation)
    (application)
    (atomic))
   )

  (@anything (i:seq* (atom '_)
                     (i:-> (make-instance 'anything-clause))))
  (quotation (i:seq*
              (list (i:seq* (i:bind q (atom 'quote))
                            (i:bind thing (i:anything))))
              (i:-> (make-instance 'atomic-clause
                                   :thing `(quote ,thing)))))
  (atomic (i:seq*
           (i:bind thing (satisfies #'atom))
           (i:-> (make-instance 'atomic-clause
                                :thing thing))))
  (application (i:seq* (list (i:seq* (i:bind rule (symbol))
                                     (i:bind args (i:many (satisfies #'atom)))))
                       (i:-> (make-instance 'application-clause
                              :rule rule
                              :args args))))
  
  (@or (i:seq* (list (i:seq* (atom 'or)
                             (i:bind clauses
                               (i:many (real-clause)))))
               (i:-> (make-instance 'or-clause
                      :bindings (remove-duplicates
                                 (loop for clause in clauses
                                      appending (bindings clause)))
                      :subclauses clauses))))
  (@many (i:seq* (list (i:seq* (atom '*)
                               (i:bind form (real-clause))))
                 (i:-> (make-instance 'many-clause
                        :subclause form))))
  (plus (i:seq* (list (i:seq* (atom '+)
                               (i:bind form (real-clause))))
                 (i:-> (make-instance 'plus-clause
                        :subclause form))))
  (@neg (i:seq* (list (i:seq* (atom '~)
                              (i:bind form (real-clause))))
                (i:-> (make-instance 'neg-clause
                       :subclause form))))
  (@bind (i:seq* (list (i:seq* (atom 'bind)
                               (i:bind var (symbol))
                               (i:bind subclause (real-clause))))
                 (i:-> (make-instance 'bind-clause
                        :bindings (cons var (bindings subclause))
                        :var var
                        :subclause subclause))))
  (@-> (i:seq* (list (i:seq* (atom '->)
                             (i:bind form (i:anything))))
               (i:-> (make-instance '->-clause
                      :subclause form))))
  (@->? (i:seq* (list (i:seq* (atom '->?)
                              (i:bind form (i:anything))))
                (i:-> (make-instance '->?-clause
                       :subclause form))))
  (@list (i:seq* (list (i:seq* (atom 'list)
                               (i:bind subclause (real-clause))))
                 (i:-> (make-instance 'list-clause
                        :bindings (bindings subclause)
                        :subclause subclause))))
  (@seq (i:seq* (list (i:seq* (atom 'seq)
                              (i:bind subclauses (i:many (real-clause)))))
                (i:-> (make-instance 'seq-clause
                       :subclauses subclauses)))))

#+nil
(defun wrap-left-recursion (rule body)
  (with-gensyms (results result stream failed)
    `(acond ((memo ,rule *stream*)
             (if (m-lr? it)
                 (align-flags-for-growing-and-failure ,rule)
                 (let ((,results (m-value it)))
                   (if (eql (first ,results) *failure-value*)
                       (apply #'signal 'match-failure (rest ,results))
                       ,results))))
            (t
             (init-memo/failure-and-align-flags-for-planting-seed ,rule)
             (multiple-value-bind (,result ,stream ,failed)
                 (handler-case (progn ,@body)
                   (match-failure ()
                     (values *failure-value* *stream* t)))
               (let ((,memo (memo ,rule *stream*)))
                 (memo-add ,rule *stream* (list ,result ,stream)
                           nil (m-lr-detected? ,memo))
                 (cond ((and (m-lr-detected? ,memo)
                             (not ,failed))
                        (let ((,reults (grow-lr ,name))))
                        ))
                 )
                 )
             ,@body))))

(defun length>= (list-1 list-2)
  (cond
    ((endp list-1)
     (endp list-2))
    ((endp list-2)
     list-1)
    (t (length>= (rest list-1) (rest list-2)))))

(defun rule-apply (rule args)
  (labels ((grow-lr ()
             (multiple-value-bind (result stream failed)
                 (handler-case (apply rule args)
                   (match-failure ()
                     (values *failure-value* *stream* t)))
               (let* ((m (memo rule *stream*))
                      (m-stream (value-stream (m-value m))))
                 (cond ((or failed
                            (length>= stream m-stream))
                        (values-list (m-value m)))
                       (t
                        (memo-add rule *stream* (list result stream)
                                  (m-lr? m) (m-lr-detected? m))
                        (grow-lr)))))))
    (acond
      ((memo rule *stream*)
       (cond ((m-lr? it)
              (memo-add name *stream* (m-value it) nil t)
              (signal 'match-failure))
             (t
              (let ((results (m-value it)))
                (if (eql (first results) *failure-value*)
                    (signal 'match-failure)
                    (values-list results))))))
      (t
       (memo-add name *stream* (list *failure-value* *stream*) t nil)
       (multiple-value-bind (result stream failed)
           (handler-case (apply rule args)
             (match-failure ()
               (values *failure-value* *stream* t)))
         (let ((m (memo rule *stream*)))
           (memo-add rule *stream* (list result stream)
                     nil (m-lr-detected? m))
           (cond
             (failed (signal 'match-failure))
             ((m-lr-detected? m)
              (multiple-value-bind (result stream)
                  (grow-lr)
                (if (eql result *failure-value*)
                    (signal 'match-failure)
                    (values result stream))))
             (t (values result stream)))))))))

#+nil
(defun wrap-memoization (rule args)
  (with-gensyms (old-memo result stream failed)
    `(let ((,old-memo (memo-copy)))
       (multiple-value-bind (,result ,stream ,failed)
           (handler-case (,rule ,@args)
             (match-failure ()
               (values *failure-value* *stream* t)))
         (unless (aand (memo ,rule *stream*)
                       (m-lr-detected? it))
           (reset-memo! old-memo))
         (if ,failed
             (signal 'match-failure)
             (values ,result ,stream))))))

(defun is-a (class-name)
  (lambda (thing)
    (eql (class-of thing)
         (find-class class-name))))

(defgeneric generate-code (clause))

(defmethod generate-code ((clause atomic-clause))
  (lambda (cont)
    (with-gensyms (item rest-results stream)
      `(if (endp *stream*)
           (signal 'match-failure)
           (let ((,item (first *stream*)))
             (if (eql ,item ,(thing clause))
                 (let ((*stream* (rest *stream*)))
                   (multiple-value-bind (,rest-results ,stream)
                       ,(funcall cont)
                     (values (cons ,item ,rest-results) ,stream)))
                 (signal 'match-failure)))))))

(defmethod generate-code ((clause application-clause))
  (lambda (cont)
    (with-gensyms (result stream)
      `(multiple-value-bind (,result ,stream)
           (rule-apply #',(rule clause) ,@(args clause))
         (let ((*stream* ,stream))
           (cons ,result ,(funcall cont)))))))

(defmethod generate-code ((clause or-clause))
  (lambda (cont)
    (with-gensyms (succeeded result block)
      `(multiple-value-bind (,succeeded ,result ,@(bindings clause))
           (block ,block
             (tagbody
                ,@(loop for c in (subclauses clause)
                     collect
                       `(return-from ,block
                          (let ((,result
                                 ,(i:omatch ast->code
                                            start (list c))))
                            (values
                             t
                             ,result
                             ,@(mapcar
                                (lambda (b)
                                  (if (member b (bindings c))
                                      b
                                      `nil))
                                (bindings clause))))))))
         (if ,succeeded
             (cons ,result ,(funcall cont))
             (signal 'match-failure))))))

(defmethod generate-code ((clause many-clause))
  (lambda (cont)
    (with-gensyms (accum result rest-results stream failed repeat)
      `(let ((,accum nil))
         (labels ((,repeat ()
                    (multiple-value-bind (,result ,stream ,failed)
                        (handler-case
                            ,(i:omatch ast->code
                                       start
                                       (list
                                        (subclause clause)))
                          (match-failure ()
                            (values *failure-value* *stream* t)))
                      (cond (,failed
                             (values (nreverse
                                      (reduce #'append ,accum)) ,stream))
                            (t
                             (push ,result ,accum)
                             (let ((*stream* ,stream))
                               (,repeat)))))))
           (multiple-value-bind (,result *stream*)
               (,repeat)
             (multiple-value-bind (,rest-results ,stream)
                 ,(funcall cont)
               (values (append ,result ,rest-results) ,stream))))))))

(defmethod generate-code ((clause plus-clause))
  (lambda (cont)
    (with-gensyms (accum result rest-results stream failed repeat)
      (let ((subclause-code
             (i:omatch ast->code
                       start
                       (list (subclause clause)))))
        `(multiple-value-bind (,result ,stream)
             ,subclause-code
           (let ((,accum ,result))
             (labels ((,repeat ()
                        (multiple-value-bind (,result ,stream ,failed)
                            (handler-case
                                ,(i:omatch ast->code
                                           start
                                           (list
                                            (subclause clause)))
                              (match-failure ()
                                (values *failure-value* *stream* t)))
                          (cond (,failed
                                 (values (nreverse
                                          (reduce #'append ,accum)) ,stream))
                                (t
                                 (push ,result ,accum)
                                 (let ((*stream* ,stream))
                                   (,repeat)))))))
               (multiple-value-bind (,result *stream*)
                   (,repeat)
                 (multiple-value-bind (,rest-results ,stream)
                     ,(funcall cont)
                   (values (append ,result ,rest-results) ,stream))))))))))

(defmethod generate-code ((clause bind-clause))
  (lambda (cont)
    (with-gensyms (result stream)
      `(multiple-value-bind (,result ,stream)
           ,(i:omatch ast->code start (list (subclause clause)))
         (setf ,(var clause) ,result)
         (let ((*stream* ,stream))
           (multiple-value-bind (,rest-results ,stream)
               ,(funcall cont)
             (values (cons ,result ,rest-results) ,stream)))))))

(defmethod generate-code ((clause ->-clause))
  (lambda (cont)
    (with-gensyms (result rest-results stream)
      `(let ((,result ,(subclause clause)))
         (multiple-value-bind (,rest-results ,stream)
             ,(funcall cont)
           (values (cons ,result ,rest-results) ,stream))))))

(defmethod generate-code ((clause ->?-clause))
  (lambda (cont)
    `(if ,(subclause clause)
         ,(funcall cont)
         (signal 'match-failure))))

(defmethod generate-code ((clause list-clause))
  (lambda (cont)
    (with-gensyms (result rest-results stream substream)
      `(cond
         ((endp *stream*)
          (signal 'match-failure))
         (t
          (let ((,substream (first *stream*)))
            (if (listp ,substream)
                (let ((*stream* ,substream))
                  (multiple-value-bind (,result ,stream)
                      ,(i:omatch ast->code start (list (subclause clause)))
                    (if (endp ,stream)
                        (multiple-value-bind (,rest-results ,stream)
                            ,(funcall cont)
                          (values (cons ,substream ,rest-results) ,stream))
                        (signal 'match-failure)))))))))))

(defmethod generate-code ((clause anything-clause))
  (lambda (cont)
    (with-gensyms (rest-results stream)
      `(if (endp *stream*)
           (signal 'match-failure)
           (multiple-value-bind (,rest-results ,stream)
               (let ((*stream* (rest *stream*)))
                 ,(funcall cont))
             (values (cons (first *stream*) ,rest-results) ,stream))))))

(defmethod generate-code ((clause neg-clause))
  (lambda (cont)
    (with-gensyms (result stream failed)
      `(multiple-value-bind (,result ,stream ,failed)
           (handler-case ,(i:omatch ast->code start (list (subclause clause)))
             (match-failure ()
               (values *failure-value* *stream* t)))
         (if ,failed
             ,(funcall cont)
             (signal 'match-failure))))))

(i:define-ometa ast->code
  (symbol (i:seq* (i:bind s (i:anything))
                  (i:->? (symbolp s))
                  (i:-> s)))
  (struct kind
          (i:seq* (i:bind s (i:anything))
                  (i:->? (and (ometa-clause-p s)
                              (or (null kind)
                                  (eql (ometa-clause-kind s) kind))))
                  (i:-> s)))
  (satisfies pred (i:seq* (i:bind s (i:anything))
                          (i:->? (funcall pred s))
                          (i:-> s)))
  (atomic
   (i:seq*
    (i:bind s (satisfies (is-a 'atomic-clause)))
    (i:->
     (make-instance 'atomic-clause
                    :bindings nil
                    :code (generate-code s)))))
  (application
   (i:seq*
    (i:bind s (satisfies (is-a 'application-clause)))
    (i:->
     (make-instance 'application-clause
                    :code (generate-code s)))))
  (@or
   (i:seq*
    (i:bind s (satisfies (is-a 'or-clause)))
    (i:-> 
     (let ((bindings (bindings s)))
       (make-instance 'or-clause
                      :bindings bindings
                      :code (generate-code s))))))
  (@many
   (i:seq* (i:bind s (satisfies (is-a 'many-clause)))
           (i:-> 
            (make-instance 'many-clause
                           :code (generate-code s)))))
  (plus
   (i:seq* (i:bind s (satisfies (is-a 'plus-clause)))
           (i:-> 
            (make-instance 'many-clause
                           :code (generate-code s)))))
  (@some)
  (@neg
   (i:seq* (i:bind s (satisfies (is-a 'neg-clause)))
           (i:-> 
            (make-instance 'neg-clause
                           :code (generate-code s)))))
  (@bind
      (i:seq* (i:bind s (satisfies (is-a 'bind-clause)))
              (i:-> 
               (make-instance 'bind-clause
                              :code (generate-code s)
                              )
               ;;(ometa-clause-form s)
               )))
  (@->
   (i:seq* (i:bind s (satisfies (is-a '->-clause)))
           (i:-> 
            (make-instance '->-clause
                           :code (generate-code s)))))
  (@->?
   (i:seq* (i:bind s (satisfies (is-a '->?-clause)))
           (i:-> 
            (make-instance '->?-clause
                           :code (generate-code s)))))
  (@list
   (i:seq* (i:bind s (satisfies (is-a 'list-clause)))
           (i:-> 
            (make-instance 'list-clause
                           :code (generate-code s)))))
  (@seq*
   (i:seq* (i:bind s (satisfies (is-a 'seq*-clause)))
           (i:-> 
            (make-instance 'seq*-clause
                           :code (generate-code s)))))
  (@anything
   (i:seq* (i:bind s (satisfies (is-a 'anything-clause)))
           (i:->
            (make-instance 'anything-clause
                           :code (generate-code s)))))
  (real-clause
   (i:seq*
    (i:bind c
      (i:alt*
       (@or)
       (@many)
       (plus)
       (@neg)
       (@bind)
       (@->)
       (@->?)
       (@->)
       (@list)
       (@anything)
       (application)
       (atomic)))
    (i:-> c)))
  (start (i:seq*
          (list
           (i:bind conts
             (i:many (i:seq* (i:bind s (real-clause))
                             (i:-> (code s))))))
          (i:-> 
           (reduce (lambda (x y)
                     (funcall x (lambda () y)))
                   conts
                   :from-end t
                   :initial-value
                   `(values nil *stream*))))))

#+nil
(reduce (lambda (x y)
          (funcall x (lambda () y)))
        (list
         (lambda (x) (list 1 (funcall x)))
         (lambda (x) (list 2 (funcall x)))
         (lambda (x) (list 3 (funcall x)))
         (lambda (x) (list 4 (funcall x))))
        :from-end t
        :initial-value (lambda () nil)
        )

#+nil
(print
 (i:omatch ometa-grammar
         start
         '((+ 'narf)))
 t)

#+nil
(let ((step1
       (i:omatch ometa-grammar
                     start
                     '((+ 'barf)))))
  (print
   (i:omatch ast->code
             start
             step1
             ;;(list  (make-ometa-clause :kind :eql :bindings nil :form '(or (bind x (eql :x)))))
             )))

#+nil
(defgrammar stuff ()
  (derp ()
        _ (* 'barf) 'narf))

(defmacro oeval (grammar rule args input)
  `(let ((*stream* ,input))
     (with-active-layers (,grammar)
           (,rule ,@args))
     #+nil
     (handler-case
         (with-active-layers (,grammar)
           (,rule ,@args))
       (match-failure ()
         nil))))
#+nil

(oeval stuff derp () (list 'narf 'barf 'barf 'barf 'narf 'scarf))

#+nil
(defgrammar std ()
  (char () 
        (bind c _)
        (->? (characterp c))
        (-> c))
  ;; why bake character-classes into a language when
  ;; its so expressive that adding them is trivial
  (char-range (x y)
              (bind c _)
              (->? (and (characterp c)
                        (char<= x c y)))
              (-> c))
  (letter () (or (char-range #\a #\z)
                 (char-range #\A #\Z)))
  (digit () (char-range #\0 #\9))
  (number () (+ (digit)))
  (spaces () (+  #\space)))

#+nil
(defgrammar simple-binding (std)
  (start ()
         (list
          (bind x (atom 1))
          ;;(bind y (atom 2))
          )
         (-> (list x y))))

#+nil
(defgrammar simple-binding ()
  (start () (list
             (bind x (atom 1))
             (bind y (atom 2)))
         :-> (list x y)))
#+nil
(defgrammar simple-binding-apply ()
  (start () (list
             (bind x (anything))
             (bind y (alt* (atom 3)
                           (start))))
         :-> (list x y)))
#+nil
(defgrammar left-recursion ()
  (start () (or ((bind x (start))
                 (atom #\-)
                 (bind y (n))
                 :-> (list 'sub x y))
                ((bind x (start))
                 (atom #\+)
                 (bind y (N))
                 :-> (list 'add x y))
                (n)))
  (n () (or (atom #\1)
            (atom #\2)
            (atom #\3))))
#+nil
(defgrammar direct-left-recursion ()
  (start (or ((bind x (start))
              (atom #\-)
              (bind y (n))
              :-> (list x y))
             (n)))
  (n (or (atom #\1)
         (atom #\2)
         (atom #\3))))
#+nil
(defgrammar empty-1 ()
  (start () (list (end)))
  (end () (~ (anything))))
#+nil
(definegrammar empty-2 ()
  (start () (list (list (end))))
  (end () (~ (anything))))
#+nil
(defgrammar empty-3 ()
  (start () (list (list (end))))
  (end () (~ (anything))))
#+nil
(defgrammar integers (std)                         ;inherit from std
  (int () (or ((bind n (int))
               (bind d (foreign (^ digit)))          ;invoke a `digit' parent rule
               :-> (+ (* n 10) (char->number d)))
              ((bind d (foreign (^ digit)))
               :-> (char->number d)))))
#+nil
(defgrammar integers (std)                         ;inherit from std
  (int () (or ((bind n (int))
               (bind d (foreign (^ digit)))          ;invoke a `digit' parent rule
               :-> (+ (* n 10) (char->number d)))
              ((bind d (foreign (^ digit)))
               :-> (char->number d)))))
#+nil
(defgrammar token (std)
  (letter () (or (eql #\_)              ; accept underscore
                 (foreign (^ letter))))    ; invoke parent rule
  (id () (+ (letter)))
  (number (or ((bind pre (foreign (^ number)))
               (eql #\.)
               (bind post (foreign (^ number)))
               :-> `(,@pre #\. ,@post))
              (foreign (^ number)))))
#+nil
(defgrammar flat ()
  (flatten () (list (bind xs (inside)))
           :-> xs)
  (inside () (or
              ((list (bind xs (inside)))
               (bind ys (inside))
               :-> (append xs ys))
              ((bind x (anything))
               (bind xs (inside))
               :-> (cons x xs))
              ((end)
               :-> nil)))
  (end () (~ (anything))
       :-> nil))
#+nil
(defgrammar toks (std)
  (eq () (atom #\=)
      :-> (hash-create `((kind =) (value "="))))
  (num () (bind n (foreign (^ number)))
       :-> (hash-create `((kind num) (value ,(list->string n)))))
  ;; not just rules inherited from `std'
  ;; let's invoke one from `token'
  (id () (bind ls (foreign (^ id token)))
      :-> (hash-create `((kind id) (value ,(list->string ls)))))
  (scanner () 
           (foreign (^ spaces))
           (or (eq)
               (num)
               (id))))
#+nil
(defgrammar assignments (toks)
  (token (k)  (bind tok (foreign (^ scanner)))
         :p (equal (gethash 'kind tok nil) k)
         :-> (gethash 'value tok))
  (assign () (bind a (token 'id))
          (bind b (token '=))
          (bind c (token 'num))
          :-> (concatenate 'string a b c)))
#+nil
(defgrammar lambda-list ()
  (symbol ()  (bind s (anything))
          :->? (symbolp s)
          :-> s)
  (llkey ()  (bind s (symbol))
         :->? (member s '(:&optional :&rest :&key :&allow-other-keys :&aux))
         :-> s)
  (required ()  (~ (llkey))
            (bind j (symbol))
            :-> (list :required j))
  (optional () (~ (llkey))
            (or (bind name (symbol))
                (list (bind name (symbol))
                      (bind init-form (anything))
                      (bind supplied-p (symbol))
                      
                  ;;; same problem as with optima:
                  ;;; how to express extending horizon of optional elements?

                      ))
            :-> (list :optional name ;;init-form supplied-p
                      )
            )
  (key () (~ (llkey))
       (or (bind name (symbol))
           (list (bind name (symbol))
                 (bind init-form (anything))
                 (bind supplied-p (symbol))))
       :-> (list :key name))
  (aux () (~ (llkey))
       (or (bind name (symbol))
           (list (bind name (symbol))
                 (bind init-form (anything)))))
  (start () (list (bind requireds (* (required)))
                  (bind optionals
                    (* ((eql '&optional)
                        (* (optional)))))
                  (bind rest
                    (? ((eql '&rest)
                        (~ (llkey))
                        (symbol))))
                  (bind keys
                    (* ((eql '&key)
                        (* (key))
                        (? (eql '&allow-other-keys)))))
                  (bind auxes
                    (? ((atom :&aux)
                        (* (aux))))))
         (-> (list requireds optionals rest keys auxes))))


#+nil
(defgrammar flat ()
  (flatten () (list (bind xs (inside)))
           :-> xs)
  (inside () (or
              ((list (bind xs (inside)))
               (bind ys (inside))
               :-> (append xs ys))
              ((bind x (anything))
               (bind xs (inside))
               :-> (cons x xs))
              ((end)
               :-> nil)))
  (end () (~ (anything))
       :-> nil))

