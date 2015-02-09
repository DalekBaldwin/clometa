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

(defparameter *memo* nil)

(defun memo (rule grammar stream)
  (acond
    ((gethash (list rule grammar) *memo*)
     (gethash stream it))))

(defun memo-add (rule grammar stream entry)
  (let ((key (list rule grammar)))
    (acond
      ((gethash key *memo*)
       (setf (gethash stream it) entry))
      (t (setf (gethash key *memo*)
               (let ((rule-memo (make-hash-table :test #'eql)))
                 (setf (gethash stream rule-memo) entry)
                 rule-memo))))))

(defun memo-copy ()
  (copy-hash-table *memo* :key #'copy-hash-table))

(defun memo-for (symbol)
  (find-symbol (symbol-name symbol) :clometa-memos))

(defun internal-symbol (rule)
  (intern (princ-to-string rule)
          (or 
           (find-package "CLOMETA-MEMOS")
           (make-package "CLOMETA-MEMOS"))))

(defmacro defrule (name grammar (&rest args) &body body)
  `(progn
     (define-layered-function ,name (,@args))
     (define-layered-method ,name
       :in ,grammar ,args
       (rule-apply ',name (list ,@args)
                   ',grammar
                   (lambda ()
                     ,(let ((ast (i:omatch ometa-grammar start body)))
                           (i:omatch ast->code start ast)))))))

(defmacro defgrammar (grammar (&optional supergrammar) &rest rules)
  `(progn
     (deflayer ,grammar ,(when supergrammar (list supergrammar)))
     ,@(loop for rule in rules
          collect
            (destructuring-bind (name (&rest args) &body body)
                rule
              `(defrule ,name ,grammar (,@args) ,@body)))))

(defclass ometa-clause ()
  ((bindings
    :accessor bindings
    :initarg :bindings
    :initform nil)
   (hoisted-bindings
    :accessor hoisted-bindings
    :initarg :hoisted-bindings
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

(defclass seq*-clause (ometa-clause)
  ((subclauses
    :accessor subclauses
    :initarg :subclauses)))

(defclass application-clause (ometa-clause)
  ((rule-form
    :accessor rule-form
    :initarg :rule-form)
   (args
    :accessor args
    :initarg :args)))

(defclass call-clause (ometa-clause)
  ((rule
    :accessor rule
    :initarg :rule)
   (args
    :accessor args
    :initarg :args)))

(defclass next-rule-clause (ometa-clause)
  ((args
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
  (application (i:seq* (list (i:seq* (atom 'apply)
                                     (i:bind rule-form (i:anything))
                                     (i:bind args (i:many (satisfies #'atom)))))
                       (i:-> (make-instance 'application-clause
                                            :rule-form rule-form
                                            :args args))))
  (call (i:seq* (list (i:seq* (i:bind rule (symbol))
                              (i:bind args (i:many (i:anything)))))
                (i:-> (make-instance 'call-clause
                                     :rule rule
                                     :args args))))
  (@or (i:seq* (list (i:seq* (atom 'or)
                             (i:bind clauses
                               (i:many (real-clause)))))
               (i:->
                (let ((hoisted-bindings
                       (remove-duplicates
                        (loop for clause in clauses
                           appending (hoisted-bindings clause)
                           appending (bindings clause)))))
                  (loop for clause in clauses
                     do (setf (hoisted-bindings clause) nil))
                  (make-instance 'or-clause
                                 :bindings nil
                                 :hoisted-bindings hoisted-bindings
                                 :subclauses clauses)))))
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
                 (i:->
                  (let ((hoisted-bindings
                         (remove-duplicates
                          (cons var
                                (append (hoisted-bindings subclause)
                                        (bindings subclause))))))
                    (setf (hoisted-bindings subclause) nil)
                    (make-instance 'bind-clause
                                   :bindings (list var)
                                   :hoisted-bindings hoisted-bindings
                                   :var var
                                   :subclause subclause)))))
  (@-> (i:seq* (atom :->)
               (i:bind form (i:anything))
               (i:-> (make-instance '->-clause
                                    :subclause form))))
  (@->? (i:seq* (atom :->?)
                (i:bind form (i:anything))
                (i:-> (make-instance '->?-clause
                                     :subclause form))))
  (@list (i:seq* (list (i:seq* (atom 'list)
                               (i:bind clauses (i:many (real-clause)))))
                 (i:->
                  (let ((hoisted-bindings
                         (remove-duplicates
                          (loop for clause in clauses
                             appending (hoisted-bindings clause)
                             appending (bindings clause)))))
                    (loop for clause in clauses
                       do (setf (hoisted-bindings clause) nil))
                    (make-instance 'list-clause
                                   :bindings nil
                                   :hoisted-bindings hoisted-bindings
                                   :subclause
                                   (make-instance 'seq*-clause :subclauses clauses))))))
  (@seq (i:seq* (list (i:seq* (atom 'seq)
                              (i:bind subclauses (i:many (real-clause)))))
                (i:-> (make-instance 'seq*-clause
                                     :subclauses subclauses))))
  (next-rule (i:seq* (list (i:seq* (atom 'next-rule)
                                   (i:bind args (i:many (i:anything)))))
                     (i:-> (make-instance 'next-rule-clause
                                          :args args))))
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
    (next-rule)
    (application)
    (quotation)
    (call)
    (atomic))
   )
  (start (i:seq* (list (i:bind clauses (i:many (real-clause))))
                 (i:-> ;;derp
                  (list (make-instance 'seq*-clause :subclauses clauses))))))

(defun length>= (list-1 list-2)
  (cond
    ((endp list-1)
     (endp list-2))
    ((endp list-2)
     list-1)
    (t (length>= (rest list-1) (rest list-2)))))

(defun contains-sublist-aux (sublist list)
  (cond
    ((endp list)
     nil)
    ((eql sublist list)
     sublist)
    (t
     (contains-sublist-aux sublist (rest list)))))

(defun contains-sublist (sublist list)
  (or (endp sublist)
      (contains-sublist-aux sublist list)))

(defun m-value (m) (first m))
(defun m-lr? (m) (second m))
(defun m-lr-detected? (m) (third m))

(defstruct head
  rule
  involved-set
  eval-set)

(defstruct lr
  seed
  rule
  head
  next)

(defstruct memo-entry
  value
  stream
  lr)

(defparameter *lr-stack* nil)
(defparameter *heads* (make-hash-table :test #'eql))

(defun rule-apply (rule args grammar body-fun)
  (labels ((recall ()
             (let ((m (memo rule grammar *stream*))
                   (h (gethash *stream* *heads*)))
               (cond
                 ((null h)
                  m)
                 ((and (null m)
                       (not (eql rule (head-rule h)))
                       (not (member rule (head-involved-set h))))
                  (make-memo-entry :value failure-value
                                   :stream *stream*
                                   :lr nil))
                 ((member rule (head-eval-set h))
                  (setf (head-eval-set h)
                        (remove rule (head-eval-set h)))
                  (multiple-value-bind (result stream)
                      (handler-case
                          (funcall body-fun)
                          ;;(apply rule args)
                        (match-failure ()
                          (values failure-value *stream*)))
                    (setf (memo-entry-value m) result
                          (memo-entry-stream m) stream
                          (memo-entry-lr m) nil)
                    m))
                 (t
                  m))))
           (grow-lr (m h)
             (setf (head-eval-set h) (copy-list (head-involved-set h))) ;; Line B
             (multiple-value-bind (result stream failed)
                 (handler-case
                     (funcall body-fun)
                     ;;(apply rule args)
                   (match-failure ()
                     (values failure-value *stream* t)))
               (let ((m-stream (memo-entry-stream m)))
                 (cond ((or failed
                            (contains-sublist m-stream stream))
                        (setf (gethash *stream* *heads*) nil) ;; Line C
                        (values (memo-entry-value m) (memo-entry-stream m)))
                       (t
                        (setf (memo-entry-value m) result
                              (memo-entry-stream m) stream
                              (memo-entry-lr m) nil)
                        (grow-lr m h))))))
           (lr-answer (m)
             (let ((h (lr-head (memo-entry-lr m)))
                   (seed (lr-seed (memo-entry-lr m))))
               (cond
                 ((eql (head-rule h) rule)
                  (setf (memo-entry-value m) seed
                        (memo-entry-lr m) nil)
                  (cond
                    ((eql seed failure-value)
                     (signal 'match-failure))
                    (t
                     (setf (gethash *stream* *heads*) h)
                     (grow-lr m h))))
                 ((eql seed failure-value)
                  (signal 'match-failure))
                 (t
                  (values seed *stream*)))))
           (setup-lr (lr)
             (when (null (lr-head lr))
               (setf (lr-head lr)
                     (make-head :rule rule
                                :involved-set nil
                                :eval-set nil)))
             (labels ((process-stack (stack)
                        (when (and stack
                                   (not (eql (lr-head stack) (lr-head lr))))
                          (setf (lr-head stack) (lr-head lr))
                          (pushnew (lr-rule stack) (head-involved-set (lr-head lr)))
                          (process-stack (lr-next stack)))))
               (process-stack *lr-stack*))))
    (let ((entry (recall)))
      (cond
        (entry
         (let ((*stream* (memo-entry-stream entry)))
           (acond
             ((memo-entry-lr entry)
              (setup-lr it)
              (if (eql (lr-seed it) failure-value)
                  (signal 'match-failure)
                  (values failure-value *stream*)))
             (t
              (if (eql (memo-entry-value entry) failure-value)
                  (signal 'match-failure)
                  (values (memo-entry-value entry) *stream*))))))
        (t
         (let* ((lr (make-lr :seed failure-value
                             :rule rule
                             :head nil
                             :next *lr-stack*))
                (entry
                 (make-memo-entry :value failure-value
                                  :stream *stream*
                                  :lr lr)))
           (memo-add rule grammar *stream* entry)
           (multiple-value-bind (result stream failed)
               (handler-case
                   (let ((*lr-stack* lr))
                     (funcall body-fun)
                     ;;(apply rule args)
                     )
                 (match-failure ()
                   (values failure-value *stream* t)))
             (setf (memo-entry-stream entry) stream)
             (cond
               ((lr-head lr)
                (setf (lr-seed lr) result)
                (lr-answer entry))
               (t
                (setf (memo-entry-value entry) result
                      (memo-entry-lr entry) nil)
                (if failed
                    (signal 'match-failure)
                    (values result stream)))))))))))

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
                     (values
                      (if (eql ,rest-results empty-value)
                          ,item
                          ,rest-results)
                      ,stream)))
                 (signal 'match-failure)))))))

(defmethod generate-code ((clause application-clause))
  (lambda (cont)
    (with-gensyms (rule result rest-results stream failed)
      `(let ((,rule ,(rule-form clause)))
         (multiple-value-bind (,result ,stream ,failed)
             (handler-case
                 (apply ,rule
                        (list ,@(args clause)))
               (match-failure ()
                 (values failure-value *stream* t)))
           (if ,failed
               (signal 'match-failure)
               (let ((*stream* ,stream))
                 (multiple-value-bind (,rest-results ,stream)
                     ,(funcall cont)
                   (values
                    (if (eql ,rest-results empty-value)
                        ,result
                        ,rest-results)
                    ,stream)))))))))

(defmethod generate-code ((clause next-rule-clause))
  (lambda (cont)
    (with-gensyms (result rest-results stream failed)
      `(multiple-value-bind (,result ,stream ,failed)
           (handler-case
               (call-next-layered-method ,@(args clause))
             (match-failure ()
               (values failure-value *stream* t)))
         (if ,failed
             (signal 'match-failure)
             (let ((*stream* ,stream))
               (multiple-value-bind (,rest-results ,stream)
                   ,(funcall cont)
                 (values
                  (if (eql ,rest-results empty-value)
                      ,result
                      ,rest-results)
                  ,stream))))))))

(defmethod generate-code ((clause call-clause))
  (lambda (cont)
    (with-gensyms (result rest-results stream failed)
      `(multiple-value-bind (,result ,stream ,failed)
           (handler-case
               (,(rule clause) ,@(args clause))
             (match-failure ()
               (values failure-value *stream* t)))
         (if ,failed
             (signal 'match-failure)
             (let ((*stream* ,stream))
               (multiple-value-bind (,rest-results ,stream)
                   ,(funcall cont)
                 (values
                  (if (eql ,rest-results empty-value)
                      ,result
                      ,rest-results)
                  ,stream))))))))

(defmethod generate-code ((clause or-clause))
  (lambda (cont)
    (with-gensyms (succeeded result rest-results stream block)
      `(let (,@(hoisted-bindings clause))
         (multiple-value-bind (,succeeded
                               ,result
                               ,stream)
             (block ,block
               (tagbody
                  ,@(loop for c in (butlast (subclauses clause))
                       for tag = (gensym "FAIL")
                       collect
                         `(return-from ,block
                            (handler-case
                                (multiple-value-bind (,result ,stream)
                                    ,(i:omatch ast->code
                                               start (list c))
                                  (values
                                   t
                                   ,result
                                   ,stream))
                              (match-failure ()
                                (go ,tag))))
                       collect tag)
                  (return-from ,block
                    ,(let ((last-clause (car (last (subclauses clause)))))
                          `(multiple-value-bind (,result ,stream)
                               ,(i:omatch ast->code
                                          start (list last-clause))
                             (values
                              t
                              ,result
                              ,stream))))))
           (if ,succeeded
               (let ((*stream* ,stream))
                 (multiple-value-bind (,rest-results ,stream)
                     ,(funcall cont)
                   (values
                    (if (eql ,rest-results empty-value)
                        ,result
                        ,rest-results)
                    ,stream)))
               (signal 'match-failure)))))))

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
                            (values failure-value *stream* t)))
                      (cond (,failed
                             (values (nreverse ,accum) ,stream))
                            (t
                             (push ,result ,accum)
                             (let ((*stream* ,stream))
                               (,repeat)))))))
           (multiple-value-bind (,result *stream*)
               (,repeat)
             (multiple-value-bind (,rest-results ,stream)
                 ,(funcall cont)
               (values
                (if (eql ,rest-results empty-value)
                    ,result
                    ,rest-results)
                ,stream))))))))

(defmethod generate-code ((clause plus-clause))
  (lambda (cont)
    (with-gensyms (accum result rest-results stream failed repeat)
      (let ((subclause-code
             (i:omatch ast->code
                       start
                       (list (subclause clause)))))
        `(multiple-value-bind (,result ,stream)
             ,subclause-code
           (let ((,accum (list ,result))
                 (*stream* ,stream))
             (labels ((,repeat ()
                        (multiple-value-bind (,result ,stream ,failed)
                            (handler-case
                                ,(i:omatch ast->code
                                           start
                                           (list
                                            (subclause clause)))
                              (match-failure ()
                                (values failure-value *stream* t)))
                          (cond (,failed
                                 (values (nreverse ,accum) ,stream))
                                (t
                                 (push ,result ,accum)
                                 (let ((*stream* ,stream))
                                   (,repeat)))))))
               (multiple-value-bind (,result *stream*)
                   (,repeat)
                 (multiple-value-bind (,rest-results ,stream)
                     ,(funcall cont)
                   (values
                    (if (eql ,rest-results empty-value)
                        ,result
                        ,rest-results)
                    ,stream))))))))))

(defmethod generate-code ((clause bind-clause))
  (lambda (cont)
    (with-gensyms (result rest-results stream)
      `(let (,@(hoisted-bindings clause))
         (multiple-value-bind (,result ,stream)
             ,(i:omatch ast->code start (list (subclause clause)))
           (setf ,(var clause) ,result)
           (let ((*stream* ,stream))
             (multiple-value-bind (,rest-results ,stream)
                 ,(funcall cont)
               (values
                (if (eql ,rest-results empty-value)
                    ,result
                    ,rest-results)
                ,stream))))))))

(defmethod generate-code ((clause ->-clause))
  (lambda (cont)
    (with-gensyms (result rest-results stream)
      `(let ((,result ,(subclause clause)))
         (multiple-value-bind (,rest-results ,stream)
             ,(funcall cont)
           (values
            (if (eql ,rest-results empty-value)
                ,result
                ,rest-results)
            ,stream))))))

(defmethod generate-code ((clause ->?-clause))
  (lambda (cont)
    (with-gensyms (result rest-results stream)
      `(let ((,result ,(subclause clause)))
         (if ,result
             (multiple-value-bind (,rest-results ,stream)
                 ,(funcall cont)
               (values
                (if (eql ,rest-results empty-value)
                    ,result
                    ,rest-results)
                ,stream))
             (signal 'match-failure))))))

(defmethod generate-code ((clause list-clause))
  (lambda (cont)
    (with-gensyms (result rest-results stream substream)
      `(cond
         ((endp *stream*)
          (signal 'match-failure))
         (t
          (let ((,substream (first *stream*)))
            (cond ((listp ,substream)
                   (let (,@(hoisted-bindings clause))
                     (multiple-value-bind (,result ,stream)
                         (let ((*stream* ,substream))
                           ,(i:omatch ast->code start (list (subclause clause))))
                       ;; remember, return original list, not list of transformations
                       (declare (ignore ,result))
                       (cond ((endp ,stream)
                              (let ((*stream* (rest *stream*)))
                                (multiple-value-bind (,rest-results ,stream)
                                    ,(funcall cont)
                                  (values
                                   (if (eql ,rest-results empty-value)
                                       ,substream
                                       ,rest-results)
                                   ,stream))))
                             (t
                              (signal 'match-failure))))))
                  (t
                   (signal 'match-failure)))))))))

(defmethod generate-code ((clause anything-clause))
  (lambda (cont)
    (with-gensyms (rest-results stream item)
      `(if (endp *stream*)
           (signal 'match-failure)
           (let ((,item (first *stream*))
                 (*stream* (rest *stream*)))
             (multiple-value-bind (,rest-results ,stream)
                 ,(funcall cont)
               (values
                (if (eql ,rest-results empty-value)
                    ,item
                    ,rest-results)
                ,stream)))))))

(defmethod generate-code ((clause neg-clause))
  (lambda (cont)
    (with-gensyms (result rest-results stream failed)
      `(multiple-value-bind (,result ,stream ,failed)
           (handler-case ,(i:omatch ast->code start (list (subclause clause)))
             (match-failure ()
               (values failure-value *stream* t)))
         (if ,failed
             (multiple-value-bind (,rest-results ,stream)
                 ,(funcall cont)
               (values
                (if (eql ,rest-results empty-value)
                    ,result
                    ,rest-results)
                ,stream))
             (signal 'match-failure))))))

(defmethod generate-code ((clause seq*-clause))
  (lambda (cont)
    `(let (,@(hoisted-bindings clause))
       ,(reduce (lambda (x y)
                  (funcall x (lambda () y)))
                (mapcar #'generate-code (subclauses clause))
                :from-end t
                :initial-value
                (funcall cont)
                ;;`(values empty-value *stream*)
                )
       #+nil
       ,(labels ((recur-generate (subclauses)
                                 (cond
                                   ((null subclauses)
                                    cont)
                                   (t
                                    (funcall (generate-code (first subclauses))
                                             (recur-generate (rest subclauses)))))))
                (recur-generate (subclauses clause))))))

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
                    :code (generate-code s)))))
  (application
   (i:seq*
    (i:bind s (satisfies (is-a 'application-clause)))
    (i:->
     (make-instance 'application-clause
                    :code (generate-code s)))))
  (call
   (i:seq*
    (i:bind s (satisfies (is-a 'call-clause)))
    (i:->
     (make-instance 'call-clause
                    :code (generate-code s)))))
  (next-rule
   (i:seq*
    (i:bind s (satisfies (is-a 'next-rule-clause)))
    (i:->
     (make-instance 'next-rule-clause
                    :code (generate-code s)))))
  (@or
   (i:seq*
    (i:bind s (satisfies (is-a 'or-clause)))
    (i:-> 
     (let ((hoisted-bindings (hoisted-bindings s)))
       (make-instance 'or-clause
                      :bindings hoisted-bindings
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
       (@seq*)
       (@or)
       (@many)
       (plus)
       (@neg)
       (@bind)
       (@->)
       (@->?)
       (@->)
       (@list)
       (next-rule)
       (application)
       (@anything)
       (call)
       (atomic)))
    (i:-> c)))
  #+nil
  (proc (i:alt* (i:seq* (i:bind s (real-clause))
                        (i:bind e (i:~ (i:anything)))
                        (i:-> (funcall (code s) (lambda () `(values empty-value *stream*)))))
                (i:seq* (i:bind s (real-clause))
                        (i:bind r (proc))
                        (i:-> (funcall (code s)
                                       (lambda () r))))))
  
  ;; use this to deal with internal seqs for list
  (proc  (i:seq* (i:bind s (real-clause))
                 (i:alt*
                  (i:seq*
                   (i:bind e (i:~ (i:anything)))
                   (i:-> (funcall (code s) (lambda () `(values empty-value *stream*)))))
                  (i:seq*
                   (i:bind r (proc))
                   (i:-> (funcall (code s)
                                (lambda () r)))))))
  (start (i:seq* (list (i:bind c (real-clause)))
                 (i:-> (funcall (code c)
                                (lambda () `(values empty-value *stream*))))))
  #+nil
  (start (i:seq*
          (list
           (i:bind result (proc)))
          (i:-> result)))
  #+nil
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
                   `(values empty-value *stream*))))))

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
           '((derp)))
 t)

#+nil
(print
 (i:omatch
  (i:ometa
   (derp (list (i:seq* (i:~ (i:anything))))))
  derp
  (list))
 t)


#+nil
(let ((step1
       (i:omatch ometa-grammar
                 start
                 '((or (bind x :x))
                   :-> x
                   ))
        ))
  (print
   (i:omatch ast->code
             start
             step1
             ;;(list  (make-ometa-clause :kind :eql :bindings nil :form '(or (bind x (eql :x)))))
             )))

#+nil
(defgrammar stuff ()
  ;;(barf () (derp :x))
  ;;(derp (x) x)
  (blarf ()
         (bind z _)
         :-> z))

(defmacro omatch (grammar rule args input)
  `(let ((*stream* ,input))
     (with-active-layers (,grammar)
       (handler-case
           (,rule ,@args)
           ;;(rule-apply #',rule (list ,@args))
         (match-failure ()
           (values failure-value *stream*))))))

(defmacro gomatch (grammar rule args input)
  `(let ((*memo* (make-hash-table :test #'equal)))
     (omatch ,grammar ,rule ,args ,input)))

#+nil
(oeval stuff blarf ()
       (list :x))

#+nil
(defgrammar std ()
  ;; why bake character-classes into a language when
  ;; its so expressive that adding them is trivial
  (char-range (x y)
              (bind c _)
              :->? (and (characterp c) (char<= x c y))
              :-> c)
  (letter () (or (char-range #\a #\z)
                 (char-range #\A #\Z)))
  (digit () (char-range #\0 #\9))
  (num () (+ (digit)))
  (spaces () (+  #\space)))

#+nil
(oeval std letter () (list #\a #\6 #\7))

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
(defgrammar direct-left-recursion ()
  (start (or (:<= x (start)
                  (atom #\-)
              (let y (n))
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
