(in-package :clometa.i)
(named-readtables:in-readtable :fare-quasiquote)

(defparameter *stream* nil)
(defparameter *store* nil)
(defparameter *rules* nil)
(defparameter *ns* nil)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-symbol-macro failure-value (load-time-value *failure-value*)))
(defvar *failure-value* (gensym "=FAILURE="))

#+nil
(defmacro omatch (omprog start input &optional ns)
  `(interp/fresh-memo
    ,omprog ',start (construct-stream ,input) nil ,@(when ns (list ns))))

;; special
(defmacro omatch (omprog start input)
  `(interp/fresh-memo
    ,omprog ',start (construct-stream ,input)))

(defmacro ometa (&rest rules)
  (let ((parents
         (when (eql (first (first rules)) :<<)
           (rest (first rules)))))
    (if parents
        `(desugar '(,@(rest rules)) (list ,@parents))
        `(desugar '(,@rules)))))

(defmacro define-ometa (name &body rules)
  `(setq ,name (ometa ,@rules)))

#+nil
(defmacro define-ometa-namespace (ns-name)
  `(progn
     (define-namespace-anchor a)
     (setq ,ns-name (namespace-anchor->namespace a))))

(defun interp/fresh-memo (omprog start-rule stream)
  (fresh-memo!)
  (let ((*stream* stream))
    (interp omprog start-rule)))

(defun failure (e failurelist)
  (cond
    ((empty? *stream*)
     (list :failure (cons (list e :end :_) failurelist) *stream* *store*))
    (t (list :failure (cons (cons e (car *stream*)) failurelist) *stream* *store*))))

(defun failure/empty ()
  (list *failure-value* *stream* *store*))

(defun anything ()
  (if (empty? *stream*)
      (signal 'match-failure nil *stream* *store*);; (failure/empty)
      (list (de-index-list (cadr (car *stream*))) (cdr *stream*) *store*)))

(defun init-memo/failure-and-align-flags-for-planting-seed (name)
  (memo-add name *stream*
            (let ((*store* (fresh-store)))
              (failure/empty))
            t))

(defun align-flags-for-growing-and-failure (name)
  (memo-add name *stream* (m-value (memo name *stream*)) nil t)
  (signal 'match-failure nil *stream* *store*)
  ;;(failure/empty)
  )


(defun grow-lr (name body)
  (multiple-value-bind (result stream store failed)
      (handler-case (e body)
        (match-failure ()
          (values *failure-value* *stream* *store* t)))
    (let* (;;(ans (e body))
           ;;(ans-stream (value-stream ans))
           (memo-entry (memo name *stream*))
           (memo-stream (value-stream (m-value memo-entry))))
      (if (or failed
              (>= (length stream) (length memo-stream)))
          (m-value memo-entry)
          (progn
            (memo-add name *stream*
                      (list result stream store)
                      (m-lr? memo-entry)
                      (m-lr-detected? memo-entry))
            (grow-lr name body))))))

(macrolet
    ((introspect (arg)
       `()
       #+nil
       `(if (eql name 'clometa.i-test::int)
            (format t "~&Debug: ~A~%" ,arg)
            ,arg
            )))
  (defun rule-apply (name args)
    (let ((r
           (acond
             ((eql name 'anything)
              (introspect "anything")
              (let ((*store* (fresh-store)))
                (anything)))
             ((memo name *stream*)
              (introspect "memo found")
              (if (m-lr? it)
                  (align-flags-for-growing-and-failure name)
                  (let ((results (m-value it)))
                    (if (eql (first results) *failure-value*)
                        (apply #'signal 'match-failure (rest results))
                        results))))
             ((find-rule-by-name name *rules* args)
              (introspect (list "rule found" name))
              (init-memo/failure-and-align-flags-for-planting-seed name)
              (introspect "memo init")
              (let ((*store* (fresh-store)))
                (multiple-value-bind (result stream store failed)
                    (handler-case (e it)
                      (match-failure ()
                        (values *failure-value* *stream* *store* t)))
                  (introspect (list "stuff" result stream store failed))
                  (let ((m (memo name *stream*)))
                    (memo-add name *stream* (list result stream store)
                              nil (m-lr-detected? m))
                    (cond ((and (m-lr-detected? m)
                                (not failed))
                           (let ((results (grow-lr name it)))
                             (if (eql (first results) *failure-value*)
                                 (apply #'signal 'match-failure (rest results))
                                 results)))
                          (failed
                           (signal 'match-failure nil stream store))
                          (t
                           (list result stream store)))))))
             (t (error "no such rule ~A" name)))))
      (values (first r) (second r) (append (third r) *store*)))))

(defmethod ometa-eval (form)
  (eval form))

(defun e (exp)
  (dispatch (car exp) exp)
  #+nil
  (match (dispatch (car exp) exp)
    ((list :failure failurelist s st)
     (failure exp failurelist))
    (result
     result)))

(define-condition match-failure () ())

(defgeneric dispatch (head exp)
  ;; new -- don't need apply
  (:method ((head symbol) exp)
    (let ((old-memo (memo-copy)))
      (multiple-value-bind (result stream store)
          (let ((*store* (fresh-store)))
            (rule-apply head (cdr exp)))
        ;;(format t "~&applied rule ~A~%" head)
        (unless (and (memo head *stream*)
                     (m-lr-detected? (memo head *stream*)))
          ;;(format t "~&resetting memo~%")
          (reset-memo! old-memo))
        ;;(format t "~&returning ~A ~A ~A~%" result stream store)
        (values result stream (append *store* store)))))
  (:method ((head (eql 'foreign)) exp)
    (let* ((rule-expr (cadr exp))
           (rule-name-temp (gensym "^RULE"))
           (rule-args (cddr exp))
           (old-memo (memo-copy)))
      (multiple-value-bind (result stream store)
          (match rule-expr
            (`(^ ,name ,from-ometa)
              (let ((*store* (fresh-store)))
                (interp/fresh-memo
                 (cons `(,rule-name-temp (,name ,@rule-args))
                       from-ometa)
                 rule-name-temp
                 *stream*))))
        (unless (and (memo rule-expr *stream*)
                     (m-lr-detected? (memo rule-expr *stream*)))
          (reset-memo! old-memo))
        (if (eql result *failure-value*)
            (signal 'match-failure)
            (values result stream (append *store* store))))))
  #+nil
  (:method ((head (eql 'apply)) exp)
    (let* ((rule-expr (cadr exp))
           (rule-name-temp (gensym "^RULE"))
           (rule-args (cddr exp))
           (old-memo (memo-copy)))
      ;;(debug-pre-apply rule-expr stream store)
      (let ((ans
             (match rule-expr
               (`(^ ,name ,from-ometa)
                 (let ((*store* (fresh-store)))
                   (interp/fresh-memo
                    (cons `(,rule-name-temp (apply ,name ,@rule-args))
                          from-ometa
                          ;;(ometa-eval from-ometa)
                          )
                    rule-name-temp
                    *stream*)))
               (rule-name
                (let ((*store* (fresh-store)))
                  (rule-apply rule-name rule-args))))))
        (unless (and (memo rule-expr *stream*)
                     (m-lr-detected? (memo rule-expr *stream*)))
          (reset-memo! old-memo))
        ;;(debug-post-apply rule-expr stream store ans)
        (append-old-store ans *store*)))) ;; what to do?
  #+nil
  (:method ((head (eql 'anything)) exp)
    (let ((*store* (fresh-store)))
      (anything)))
  (:method ((head (eql 'empty)) exp)
    (values :none *stream* *store*))
  (:method ((head (eql 'seq)) exp)
    (multiple-value-match (e (second exp))
      ((_ stream store)
       (let ((*stream* stream)
             (*store* store))
         (e (third exp))
         ))
      ;;(failure failure)
      ))
  (:method ((head (eql 'atom)) exp)
    (flet ((a? (b) (equal b (cadr exp)))) ;; should this be eql??
      (multiple-value-match (e `(anything))
        (((guard a (a? a)) stream store)
         (values a stream store))
        ((_) (signal 'match-failure nil *stream* *store*)))))
  (:method ((head (eql 'alt)) exp)
    (handler-case (e (second exp))
      (match-failure ()
        (e (third exp))
        )
      (:no-error (result stream store)
        (values result stream store))))
  (:method ((head (eql 'many)) exp)
    (handler-case (e (second exp))
      (match-failure ()
        (values nil *stream* *store*))
      (:no-error (result stream store)
        (let ((*stream* stream)
              (*store* store))
          (multiple-value-bind (accum-results accum-stream accum-store)
              (e `(many ,(second exp)))
            (values (cons result accum-results)
                    accum-stream
                    accum-store))))))
  (:method ((head (eql 'many1)) exp)
    (multiple-value-bind (result stream store)
        (e `(many ,(second exp)))
      (if (null result)
          (signal 'match-failure nil stream store)
          (values result stream store))))
  (:method ((head (eql '~)) exp)
    (handler-case (e (second exp))
      (match-failure ()
        (values :none *stream* *store*))
      (:no-error (result stream store)
        (declare (ignorable result))
        (signal 'match-failure nil stream store))))
  (:method ((head (eql 'bind)) exp)
    (multiple-value-bind (result stream store)
        (e (third exp))
      (values result stream (cons (list (second exp) result) store))))
  (:method ((head (eql '->)) exp)
    (let* ((env (store->env *store*))
           (code (second exp)))
      (multiple-value-bind (result stream store)
          (ometa-eval
           `(let* ,(reverse env)
              (declare (ignorable
                        ,@(mapcar #'first env)))
              ,code))
        (declare (ignorable stream store))
        (values result *stream* *store*))))
  (:method ((head (eql '->?)) exp)
    (let* ((env (store->env *store*))
           (code (second exp)))
      (multiple-value-bind (result stream store)
          (ometa-eval
           `(let* ,(reverse env)
              (declare (ignorable
                        ,@(mapcar #'first env)))
              ,code))
        (if result
            (values :none *stream* *store*)
            ;;(failure/empty)
            (signal 'match-failure nil stream store)))))
  (:method ((head (eql 'list)) exp)
    (let* ((temprule (gensym "RULE"))
           (list-pattern (second exp))
           (subprog (cons (list temprule list-pattern) *rules*)))
      (if (empty? *stream*)
          ;;(failure/empty)
          (signal 'match-failure nil *stream* *store*)
          (match (car *stream*)
            ((list pos (guard substream (stream? substream)))
             (multiple-value-match
                 (let ((*stream* substream))
                   (interp subprog temprule))
               ((_ (guard stream (empty? stream)) store)
                (values (de-index-list substream)
                        (cdr *stream*)
                        store))
               ((_) (signal 'match-failure nil substream *store*))))
            ((list pos (satisfies atom))
             (signal 'match-failure nil *stream* *store*))
            (oops (error "Stream cell must contain a value: ~A"
                         (car *stream*))))))))

(defun interp (omprog start)
  (let ((rules omprog)
        (*rules* omprog))
    ;;(e `(,start))
    ;;#+nil
    (handler-case (e `(,start))
      (match-failure (c)
        (values *failure-value* *stream* *store*)))))

(defun matched! (thing)
  (format t "~&Matched: ~A~%" thing))

(defun desugar-e (e &optional (i nil))
  (match e
    (`(seq* ,e1) (desugar-e e1 i))
    (`(seq* ,e1 ,e2) `(seq ,(desugar-e e1 i) ,(desugar-e e2 i)))
    (`(seq* ,e1 ,@e2)
      `(seq ,(desugar-e e1 i)
            ,(desugar-e `(seq* ,@e2) i)))
    (`(alt* ,e1) (desugar-e e1 i))
    (`(alt* ,e1 ,e2)
      `(alt ,(desugar-e e1 i) ,(desugar-e e2 i)))
    (`(alt* ,e1 ,@e2)
      `(alt ,(desugar-e e1 i) ,(desugar-e `(alt* ,@e2) i)))
    (`(many ,e1)
      `(many ,(desugar-e e1 i)))
    (`(many1 ,e1)
      `(many1 ,(desugar-e e1 i)))
    (`(many+ ,e1)
      (let ((a (gensym "+A"))
                        (rest (gensym "+REST"))
                        (body (desugar-e e1 i)))
                    (desugar-e `(seq* (bind ,a ,body)
                                      (bind ,rest (many ,body))
                                      (-> (cons ,a ,rest)))
                               i)))
    (`(apply (^ ,rule-name) ,@args)
      `(apply (^ ,rule-name ,@i) ,@args))
    (`(foreign (^ ,rule-name) ,@args)
      `(foreign (^ ,rule-name ,@i) ,@args))
    (`(bind ,id ,e1)
      `(bind ,id ,(desugar-e e1 i)))
    (`(~ ,e1)
      `(~ ,(desugar-e e1 i)))
    (`(list ,e1)
      `(list ,(desugar-e e1 i)))
    (rest
     rest)))

(defun desugar-rule (rule i)
  (match rule
    (`(,name ,@ids-and-body)
      `(,name ,@(butlast ids-and-body)
              ,(desugar-e (first (last ids-and-body)) i)))
    (_ (error "Bad syntax in rule ~A" rule))))

(defun desugar (omprog &optional (i nil))
  (mapcar (lambda (rule) (desugar-rule rule i)) omprog))

(defun find-rule-by-name (name rules &optional (args nil))
  (acond
    ((assoc name rules)
     (match it
       (`(,name_ ,@ids-and-body)
         (flet ((bind (id arg) `(bind ,id (-> ,arg))))
           (desugar-e `(seq* ,@(mapcar #'bind (butlast ids-and-body) args)
                             ,@(last ids-and-body)))))
       (_ (error "Bad syntax in rule ~A" it))))
    (t nil)))
