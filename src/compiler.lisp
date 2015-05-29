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

(defun match-failure () (signal 'match-failure))

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

(defmacro chain-parsers (grammar-rules input)
  (labels ((chain (grammars)
             (cond
               ((endp grammars)
                input)
               (t
                `(clometa.i:omatch
                  ,@(first grammars)
                  ,(chain (rest grammars)))))))
    (chain (reverse grammar-rules))))

(defmacro defrule (name grammar (&rest args) &body body)
  (with-gensyms (result stream failed old-memo)
    `(progn
       (define-layered-function ,name (,@args))
       (define-layered-method ,name
         :in ,grammar ,args
         (let ((,old-memo (memo-copy)))
           (multiple-value-bind (,result ,stream ,failed)
               (handler-case
                   (rule-apply ',name (list ,@args)
                               ',grammar
                               (lambda ()
                                 ,(chain-parsers ((ometa-grammar start)
                                                  (ast->code start))
                                                 body)))
                 (match-failure ()
                   (values failure-value *stream* t)))
             
             ;; can we unwind-protect instead of explicitly catching signals
             ;; to do memo bookkeeping?
             (unless (aand (memo ',name ',grammar ,stream)
                           (memo-entry-lr it))
               ;; restore memo so same rule can be tried with different args
               (setf *memo* ,old-memo))
             (if ,failed
                 (match-failure)
                 (values ,result ,stream))))))))

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

(defclass ?-clause (ometa-clause)
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

(defclass cons-clause (ometa-clause)
  ((car-clause
    :accessor car-clause
    :initarg :car-clause)
   (cdr-clause
    :accessor cdr-clause
    :initarg :cdr-clause)))

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

(defclass foreign-clause (ometa-clause)
  ((grammar
    :accessor grammar
    :initarg :grammar)
   (rule
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

(clometa.i:define-ometa ometa-grammar
  (symbol (clometa.i:seq* (clometa.i:bind s (clometa.i:anything))
                  (clometa.i:->? (symbolp s))
                  (clometa.i:-> s)))
  (satisfies pred (clometa.i:seq* (clometa.i:bind s (clometa.i:anything))
                          (clometa.i:->? (funcall pred s))
                          (clometa.i:-> s)
                          ))


  (@anything (clometa.i:seq* (atom '_)
                     (clometa.i:-> (make-instance 'anything-clause))))
  (quotation (clometa.i:seq*
              (list (clometa.i:seq* (clometa.i:bind q (atom 'quote))
                            (clometa.i:bind thing (clometa.i:anything))))
              (clometa.i:-> (make-instance 'atomic-clause
                                   :thing `(quote ,thing)))))
  (atomic (clometa.i:seq*
           (clometa.i:bind thing (satisfies #'atom))
           (clometa.i:-> (make-instance 'atomic-clause
                                :thing thing))))
  (application (clometa.i:seq* (list (clometa.i:seq* (atom 'apply)
                                     (clometa.i:bind rule-form (clometa.i:anything))
                                     (clometa.i:bind args (clometa.i:many (satisfies #'atom)))))
                       (clometa.i:-> (make-instance 'application-clause
                                            :rule-form rule-form
                                            :args args))))
  (call (clometa.i:seq* (list (clometa.i:seq* (clometa.i:bind rule (symbol))
                              (clometa.i:bind args (clometa.i:many (clometa.i:anything)))))
                (clometa.i:-> (make-instance 'call-clause
                                     :rule rule
                                     :args args))))
  (foreign (clometa.i:seq* (list (clometa.i:seq* (atom 'foreign)
                                 (clometa.i:bind grammar (symbol))
                                 (clometa.i:bind rule (symbol))
                                 (clometa.i:bind args (clometa.i:many (clometa.i:anything)))))
                   (clometa.i:-> (make-instance 'foreign-clause
                                        :grammar grammar
                                        :rule rule
                                        :args args))))
  (macro (clometa.i:seq* (list (clometa.i:seq*
                                (clometa.i:bind head (symbol))
                                (clometa.i:->? (macro-function head))
                                (clometa.i:bind rest (clometa.i:many
                                                      (clometa.i:anything)))))
                         (clometa.i:->
                          (clometa.i:omatch ometa-grammar real-clause
                                             (macroexpand (cons head rest))))))
  (@or (clometa.i:seq* (list (clometa.i:seq* (atom 'or)
                             (clometa.i:bind clauses
                               (clometa.i:many (real-clause)))))
               (clometa.i:->
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
  (@many (clometa.i:seq* (list (clometa.i:seq* (atom '*)
                               (clometa.i:bind form (real-clause))))
                 (clometa.i:-> (make-instance 'many-clause
                                      :subclause form))))
  (plus (clometa.i:seq* (list (clometa.i:seq*
                               (atom '+)
                               (clometa.i:bind form (real-clause))))
                        (clometa.i:-> (make-instance 'plus-clause
                                     :subclause form))))
  (? (clometa.i:seq* (list (clometa.i:seq*
                            (atom '?)
                            (clometa.i:bind clauses (clometa.i:many (real-clause)))))
                     (clometa.i:->
                      (let ((hoisted-bindings
                             (remove-duplicates
                              (loop for clause in clauses
                                   appending (hoisted-bindings clause)
                                   appending (bindings clause)))))
                        (loop for clause in clauses
                             do (setf (hoisted-bindings clause) nil))
                        (make-instance '?-clause
                                       :bindings nil
                                       :hoisted-bindings hoisted-bindings
                                       :subclause
                                       (make-instance 'seq*-clause
                                                      :subclauses clauses))))))
  (@neg (clometa.i:seq* (list (clometa.i:seq*
                               (atom '~)
                               (clometa.i:bind form (real-clause))))
                        (clometa.i:-> (make-instance 'neg-clause
                                                     :subclause form))))
  (@bind (clometa.i:seq* (list (clometa.i:seq* (atom 'bind)
                               (clometa.i:bind var (symbol))
                               (clometa.i:bind subclause (real-clause))))
                 (clometa.i:->
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
  (@-> (clometa.i:seq* (atom :->)
               (clometa.i:bind form (clometa.i:anything))
               (clometa.i:-> (make-instance '->-clause
                                    :subclause form))))
  (@->? (clometa.i:seq* (atom :->?)
                (clometa.i:bind form (clometa.i:anything))
                (clometa.i:-> (make-instance '->?-clause
                                     :subclause form))))
  (cons (clometa.i:seq* (list (clometa.i:seq* (atom 'cons)
                                               (clometa.i:bind car (real-clause))
                                               (clometa.i:bind cdr (real-clause))))
                        (clometa.i:->
                         (let ((hoisted-bindings
                                (remove-duplicates
                                 (append (hoisted-bindings car)
                                         (bindings car)
                                         (hoisted-bindings cdr)
                                         (bindings cdr)))))
                           (setf (hoisted-bindings car) nil
                                 (hoisted-bindings cdr) nil)
                           (make-instance 'cons-clause
                                          :bindings nil
                                          :hoisted-bindings hoisted-bindings
                                          :car-clause car
                                          :cdr-clause cdr)))))
  (@list (clometa.i:seq* (list (clometa.i:seq* (atom 'list)
                               (clometa.i:bind clauses (clometa.i:many (real-clause)))))
                 (clometa.i:->
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
  (@seq (clometa.i:seq* (list (clometa.i:seq* (atom 'seq)
                              (clometa.i:bind subclauses (clometa.i:many (real-clause)))))
                (clometa.i:-> (make-instance 'seq*-clause
                                     :subclauses subclauses))))
  (next-rule (clometa.i:seq* (list (clometa.i:seq* (atom 'next-rule)
                                   (clometa.i:bind args (clometa.i:many (clometa.i:anything)))))
                     (clometa.i:-> (make-instance 'next-rule-clause
                                          :args args))))
  (real-clause
   (clometa.i:alt*
    (@or)
    (@many)
    (plus)
    (?)
    (@neg)
    (@bind)
    (@->)
    (@->?)
    (cons)
    (@list)
    (@seq)
    (@anything)
    (next-rule)
    (foreign)
    (application)
    (quotation)
    (macro)
    (call)
    (atomic))
   )
  (start (clometa.i:seq* (list (clometa.i:bind clauses (clometa.i:many (real-clause))))
                 (clometa.i:-> ;;derp
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
                     (match-failure))
                    (t
                     (setf (gethash *stream* *heads*) h) ;; Line A
                     (grow-lr m h))))
                 ((eql seed failure-value)
                  (match-failure))
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
                  (match-failure)
                  (values failure-value *stream*)))
             (t
              (if (eql (memo-entry-value entry) failure-value)
                  (match-failure)
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
                    (match-failure)
                    (values result stream)))))))))))

(defun is-a (class-name)
  (lambda (thing)
    (eql (class-of thing)
         (find-class class-name))))

(defgeneric generate-code (clause))

(defmethod generate-code ((clause atomic-clause))
  (lambda (cont)
    (with-gensyms (item stream)
      `(if (endp *stream*)
           (match-failure)
           (let ((,item (first *stream*)))
             (if (eql ,item ,(thing clause))
                 (let ((*stream* (rest *stream*)))
                   ,(alet (funcall cont)
                          (if (eql it empty-value)
                              `(values ,item *stream*)
                              it)))
                 (match-failure)))))))

(defmethod generate-code ((clause application-clause))
  (lambda (cont)
    (with-gensyms (rule result stream)
      `(let ((,rule ,(rule-form clause)))
         (multiple-value-bind (,result ,stream)
             (apply ,rule
                    (list ,@(args clause)))
           (declare (ignorable ,result))
           (let ((*stream* ,stream))
             ,(alet (funcall cont)
                    (if (eql it empty-value)
                        `(values ,result *stream*)
                        it))))))))

(defmethod generate-code ((clause next-rule-clause))
  (lambda (cont)
    (with-gensyms (result stream)
      `(multiple-value-bind (,result ,stream)
           (call-next-layered-method ,@(args clause))
         (declare (ignorable ,result))
         (let ((*stream* ,stream))
           ,(alet (funcall cont)
                  (if (eql it empty-value)
                      `(values ,result *stream*)
                      it)))))))

(defmethod generate-code ((clause call-clause))
  (lambda (cont)
    (with-gensyms (result stream)
      `(multiple-value-bind (,result ,stream)
           (,(rule clause) ,@(args clause))
         (declare (ignorable ,result))
         (let ((*stream* ,stream))
           ,(alet (funcall cont)
                  (if (eql it empty-value)
                      `(values ,result *stream*)
                      it)))))))

(defmethod generate-code ((clause foreign-clause))
  (lambda (cont)
    (with-gensyms (result stream)
      `(multiple-value-bind (,result ,stream)
           (with-active-layers (,(grammar clause))
             (,(rule clause) ,@(args clause)))
         (declare (ignorable ,result))
         (let ((*stream* ,stream))
           ,(alet (funcall cont)
                  (if (eql it empty-value)
                      `(values ,result *stream*)
                      it)))))))

(defmethod generate-code ((clause or-clause))
  (lambda (cont)
    (with-gensyms (result stream block)
      `(let (,@(hoisted-bindings clause))
         (multiple-value-bind (,result ,stream)
             (block ,block
               (tagbody
                  ,@(loop for c in (butlast (subclauses clause))
                       for tag = (gensym "FAIL")
                       collect
                         `(return-from ,block
                            (handler-case
                                (multiple-value-bind (,result ,stream)
                                    ,(clometa.i:omatch ast->code
                                                       start (list c))
                                  (values ,result ,stream))
                              (match-failure ()
                                (go ,tag))))
                       collect tag)
                  (return-from ,block
                    ,(let ((last-clause (car (last (subclauses clause)))))
                          `(multiple-value-bind (,result ,stream)
                               ,(clometa.i:omatch ast->code
                                                  start (list last-clause))
                             (values ,result ,stream))))))
           (declare (ignorable ,result))
           (let ((*stream* ,stream))
             ,(alet (funcall cont)
                    (if (eql it empty-value)
                        `(values ,result *stream*)
                        it))))))))

(defmethod generate-code ((clause many-clause))
  (lambda (cont)
    (with-gensyms (accum result stream failed repeat)
      `(let ((,accum nil))
         (labels ((,repeat ()
                    (multiple-value-bind (,result ,stream ,failed)
                        (handler-case
                            ,(clometa.i:omatch ast->code
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
             (declare (ignorable ,result))
             ,(alet (funcall cont)
                    (if (eql it empty-value)
                        `(values ,result *stream*)
                        it))))))))

(defmethod generate-code ((clause plus-clause))
  (lambda (cont)
    (with-gensyms (accum result stream failed repeat)
      (let ((subclause-code
             (clometa.i:omatch ast->code
                               start
                               (list (subclause clause)))))
        `(multiple-value-bind (,result ,stream)
             ,subclause-code
           (let ((,accum (list ,result))
                 (*stream* ,stream))
             (labels ((,repeat ()
                        (multiple-value-bind (,result ,stream ,failed)
                            (handler-case
                                ,(clometa.i:omatch ast->code
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
                 (declare (ignorable ,result))
                 ,(alet (funcall cont)
                        (if (eql it empty-value)
                            `(values ,result *stream*)
                            it))))))))))

(defmethod generate-code ((clause ?-clause))
  (lambda (cont)
    (with-gensyms (result failed)
      `(let (,@(hoisted-bindings clause))
         (multiple-value-bind (,result *stream* ,failed)
             (handler-case
                 ,(clometa.i:omatch ast->code
                                    start
                                    (list (subclause clause)))
               (match-failure ()
                 (values failure-value *stream* t)))
           (declare (ignorable ,result))
           (when ,failed
             (setf ,@(loop for binding in (hoisted-bindings clause)
                        collect binding
                        collect `nil)))
           ,(alet (funcall cont)
                  (if (eql it empty-value)
                      `(values (if ,failed
                                   nil
                                   (list ,result)) *stream*)
                      it)))))))

(defmethod generate-code ((clause bind-clause))
  (lambda (cont)
    (with-gensyms (result stream)
      `(let (,@(hoisted-bindings clause))
         (multiple-value-bind (,result ,stream)
             ,(clometa.i:omatch ast->code start (list (subclause clause)))
           (setf ,(var clause) ,result)
           (let ((*stream* ,stream))
             ,(alet (funcall cont)
                    (if (eql it empty-value)
                        `(values ,result *stream*)
                        it))))))))

(defmethod generate-code ((clause ->-clause))
  (lambda (cont)
    (with-gensyms (result stream)
      `(let ((,result ,(subclause clause)))
         (declare (ignorable ,result))
         ,(alet (funcall cont)
                (if (eql it empty-value)
                    `(values ,result *stream*)
                    it))))))

(defmethod generate-code ((clause ->?-clause))
  (lambda (cont)
    (with-gensyms (result stream)
      `(let ((,result ,(subclause clause)))
         (if ,result
             ,(alet (funcall cont)
                    (if (eql it empty-value)
                        `(values ,result *stream*)
                        it))
             (match-failure))))))

(defmethod generate-code ((clause cons-clause))
  (lambda (cont)
    (with-gensyms (stream-head stream-rest result stream)
      `(cond
         ((endp *stream*)
          (match-failure))
         (t
          (let ((,stream-head (first *stream*)))
            (cond ((consp ,stream-head)
                   (let ((,stream-rest (rest *stream*))
                         ,@(hoisted-bindings clause))
                     (let ((*stream* (list (car ,stream-head))))
                       (multiple-value-bind (,result ,stream)
                           ,(clometa.i:omatch ast->code start (list (car-clause clause)))
                         (declare (ignore ,result))
                         (cond ((endp ,stream)
                                (let ((*stream* (list (cdr ,stream-head))))
                                  (multiple-value-bind (,result ,stream)
                                      ,(clometa.i:omatch ast->code start
                                                         (list (cdr-clause clause)))
                                    (declare (ignore ,result))
                                    (cond ((endp ,stream)
                                           (let ((*stream* ,stream-rest))
                                             ,(alet (funcall cont)
                                                    (if (eql it empty-value)
                                                        `(values ,stream-head *stream*)
                                                        it))))
                                          (t
                                           (match-failure))))))
                               (t
                                (match-failure)))))))
                  (t
                   (match-failure)))))))))

(defmethod generate-code ((clause list-clause))
  (lambda (cont)
    (with-gensyms (result stream substream)
      `(cond
         ((endp *stream*)
          (match-failure))
         (t
          (let ((,substream (first *stream*)))
            (cond ((listp ,substream)
                   (let (,@(hoisted-bindings clause))
                     (multiple-value-bind (,result ,stream)
                         (let ((*stream* ,substream))
                           ,(clometa.i:omatch ast->code start (list (subclause clause))))
                       ;; remember, return original list, not list of transformations
                       (declare (ignore ,result))
                       (cond ((endp ,stream)
                              (let ((*stream* (rest *stream*)))
                                ,(alet (funcall cont)
                                       (if (eql it empty-value)
                                           `(values ,substream *stream*)
                                           it))))
                             (t
                              (match-failure))))))
                  (t
                   (match-failure)))))))))

(defmethod generate-code ((clause anything-clause))
  (lambda (cont)
    (with-gensyms (stream item)
      `(if (endp *stream*)
           (match-failure)
           (let ((,item (first *stream*))
                 (*stream* (rest *stream*)))
             ,(alet (funcall cont)
                    (if (eql it empty-value)
                        `(values ,item *stream*)
                        it)))))))

(defmethod generate-code ((clause neg-clause))
  (lambda (cont)
    (with-gensyms (result stream failed)
      `(multiple-value-bind (,result ,stream ,failed)
           (handler-case ,(clometa.i:omatch ast->code start (list (subclause clause)))
             (match-failure ()
               (values failure-value *stream* t)))
         (declare (ignorable ,result))
         (if ,failed
             ,(alet (funcall cont)
                    (if (eql it empty-value)
                        `(values ,result *stream*)
                        it))
             (match-failure))))))

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

(clometa.i:define-ometa ast->code
  (symbol (clometa.i:seq* (clometa.i:bind s (clometa.i:anything))
                  (clometa.i:->? (symbolp s))
                  (clometa.i:-> s)))
  (struct kind
          (clometa.i:seq* (clometa.i:bind s (clometa.i:anything))
                  (clometa.i:->? (and (ometa-clause-p s)
                              (or (null kind)
                                  (eql (ometa-clause-kind s) kind))))
                  (clometa.i:-> s)))
  (satisfies pred (clometa.i:seq* (clometa.i:bind s (clometa.i:anything))
                          (clometa.i:->? (funcall pred s))
                          (clometa.i:-> s)))
  (atomic
   (clometa.i:seq*
    (clometa.i:bind s (satisfies (is-a 'atomic-clause)))
    (clometa.i:->
     (make-instance 'atomic-clause
                    :code (generate-code s)))))
  (application
   (clometa.i:seq*
    (clometa.i:bind s (satisfies (is-a 'application-clause)))
    (clometa.i:->
     (make-instance 'application-clause
                    :code (generate-code s)))))
  (call
   (clometa.i:seq*
    (clometa.i:bind s (satisfies (is-a 'call-clause)))
    (clometa.i:->
     (make-instance 'call-clause
                    :code (generate-code s)))))
  (next-rule
   (clometa.i:seq*
    (clometa.i:bind s (satisfies (is-a 'next-rule-clause)))
    (clometa.i:->
     (make-instance 'next-rule-clause
                    :code (generate-code s)))))
  (foreign
   (clometa.i:seq*
    (clometa.i:bind s (satisfies (is-a 'foreign-clause)))
    (clometa.i:->
     (make-instance 'foreign-clause
                    :code (generate-code s)))))
  (@or
   (clometa.i:seq*
    (clometa.i:bind s (satisfies (is-a 'or-clause)))
    (clometa.i:-> 
     (let ((hoisted-bindings (hoisted-bindings s)))
       (make-instance 'or-clause
                      :bindings hoisted-bindings
                      :code (generate-code s))))))
  (@many
   (clometa.i:seq* (clometa.i:bind s (satisfies (is-a 'many-clause)))
           (clometa.i:-> 
            (make-instance 'many-clause
                           :code (generate-code s)))))
  (plus
   (clometa.i:seq* (clometa.i:bind s (satisfies (is-a 'plus-clause)))
           (clometa.i:-> 
            (make-instance 'many-clause
                           :code (generate-code s)))))
  (?
   (clometa.i:seq* (clometa.i:bind s (satisfies (is-a '?-clause)))
                   (clometa.i:->
                    (make-instance '?-clause
                                   :code (generate-code s)))))
  (@some)
  (@neg
   (clometa.i:seq* (clometa.i:bind s (satisfies (is-a 'neg-clause)))
           (clometa.i:-> 
            (make-instance 'neg-clause
                           :code (generate-code s)))))
  (@bind
      (clometa.i:seq* (clometa.i:bind s (satisfies (is-a 'bind-clause)))
              (clometa.i:-> 
               (make-instance 'bind-clause
                              :code (generate-code s)
                              )
               ;;(ometa-clause-form s)
               )))
  (@->
   (clometa.i:seq* (clometa.i:bind s (satisfies (is-a '->-clause)))
           (clometa.i:-> 
            (make-instance '->-clause
                           :code (generate-code s)))))
  (@->?
   (clometa.i:seq* (clometa.i:bind s (satisfies (is-a '->?-clause)))
           (clometa.i:-> 
            (make-instance '->?-clause
                           :code (generate-code s)))))
  (cons
   (clometa.i:seq* (clometa.i:bind s (satisfies (is-a 'cons-clause)))
                   (clometa.i:->
                    (make-instance 'cons-clause
                                   :code (generate-code s)))))
  (@list
   (clometa.i:seq* (clometa.i:bind s (satisfies (is-a 'list-clause)))
           (clometa.i:-> 
            (make-instance 'list-clause
                           :code (generate-code s)))))
  (@seq*
   (clometa.i:seq* (clometa.i:bind s (satisfies (is-a 'seq*-clause)))
           (clometa.i:-> 
            (make-instance 'seq*-clause
                           :code (generate-code s)))))
  (@anything
   (clometa.i:seq* (clometa.i:bind s (satisfies (is-a 'anything-clause)))
           (clometa.i:->
            (make-instance 'anything-clause
                           :code (generate-code s)))))
  (real-clause
   (clometa.i:seq*
    (clometa.i:bind c
      (clometa.i:alt*
       (@seq*)
       (@or)
       (@many)
       (plus)
       (?)
       (@neg)
       (@bind)
       (@->)
       (@->?)
       (@->)
       (cons)
       (@list)
       (next-rule)
       (application)
       (foreign)
       (@anything)
       (call)
       (atomic)))
    (clometa.i:-> c)))
  
  ;; use this to deal with internal seqs for list
  (proc  (clometa.i:seq* (clometa.i:bind s (real-clause))
                 (clometa.i:alt*
                  (clometa.i:seq*
                   (clometa.i:bind e (clometa.i:~ (clometa.i:anything)))
                   (clometa.i:-> (funcall (code s) (lambda () `(values empty-value *stream*)))))
                  (clometa.i:seq*
                   (clometa.i:bind r (proc))
                   (clometa.i:-> (funcall (code s)
                                (lambda () r)))))))
  (start (clometa.i:seq* (list (clometa.i:bind c (real-clause)))
                         (clometa.i:-> (funcall (code c) (lambda () empty-value))))))

(defmacro omatch% (grammar rule args input)
  `(let ((*stream* ,input))
     (with-active-layers (,grammar)
       (handler-case
           (,rule ,@args)
           ;;(rule-apply #',rule (list ,@args))
         (match-failure ()
           (values failure-value *stream*))))))

(defmacro omatch (grammar rule args input)
  `(let ((*memo* (make-hash-table :test #'equal)))
     (clrhash *heads*)
     (omatch% ,grammar ,rule ,args ,input)))
