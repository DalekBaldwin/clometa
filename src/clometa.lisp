(in-package :clometa)
(named-readtables:in-readtable :fare-quasiquote)

(defparameter *stream* nil)
(defparameter *store* nil)
(defparameter *rules* nil)
(defparameter *ns* nil)

(defmacro omatch (omprog start input &optional ns)
  `(interp/fresh-memo
    ,omprog ',start (construct-stream ,input) nil ,@(when ns (list ns))))

(defmacro ometa (&rest rules)
  (let ((parents
         (when (eql (first (first rules)) :<<)
           (rest (first rules)))))
    (if parents
        `(desugar '(,@(rest rules)) '(,@parents))
        `(desugar '(,@rules)))))

(defmacro define-ometa (name &rest rules)
  `(setq ,name (ometa ,@rules)))

#+nil
(defmacro define-ometa-namespace (ns-name)
  `(progn
     (define-namespace-anchor a)
     (setq ,ns-name (namespace-anchor->namespace a))))

(defun interp/fresh-memo (omprog start-rule stream &optional store ns)
  (fresh-memo!)
  (interp omprog start-rule stream store ns))

(defun failure (e stream store failurelist)
  (cond
    ((empty? stream)
     (list :failure (cons (list e :end :_) failurelist) stream store))
    (t (list :failure (cons (cons e (car stream)) failurelist) stream store))))

(defun failure/empty (stream store)
  (list :failure nil stream store))

(defun anything (stream store)
  (if (empty? stream)
      (failure/empty stream store)
      (list (de-index-list (cadr (car stream))) (cdr stream) store)))

(defun init-memo/failure-and-align-flags-for-planting-seed (name stream)
  (memo-add name stream (failure/empty stream (fresh-store)) t))

(defun align-flags-for-growing-and-failure (name stream store)
  (memo-add name stream (m-value (memo name stream)) nil t)
  (failure/empty stream store))

(defun rule-apply (name args stream store)
  (flet ((left-recursion? ()
           (m-lr? (memo name stream)))
         (grow-lr (body)
           (let* ((ans (e body stream store))
                  (ans-stream (value-stream ans))
                  (memo-entry (memo name stream))
                  (memo-stream (value-stream (m-value memo-entry))))
             (if (or (failure? ans)
                     (>= (length ans-stream) (length memo-stream)))
                 (m-value memo-entry)
                 (progn
                   (memo-add name stream ans (m-lr? memo-entry)
                             (m-lr-detected? memo-entry))
                   (grow-lr body))))))
    (let ((r (reverse
              (acond
                ((eql name 'anything) (anything stream (fresh-store)))
                ((memo name stream)
                 (if (left-recursion?)
                     (align-flags-for-growing-and-failure name stream store)
                     (m-value it)))
                ((find-rule-by-name name *rules* args)
                 (init-memo/failure-and-align-flags-for-planting-seed name stream)
                 (let ((ans (e it stream (fresh-store)))
                       (m (memo name stream)))
                   (memo-add name stream ans nil (m-lr-detected? m))
                   (if (and (m-lr-detected? m)
                            (not (failure? ans)))
                       (grow-lr it)
                       ans)))
                (t (error "no such rule ~A" name))))))
      (reverse (cons (append (car r) store) (cdr r))))))

(defun ometa-eval (form)
  (eval form))

(defun e (exp stream store)
  (match
      (case (car exp)
        ((apply)
         (let* ((rule-expr (cadr exp))
                (rule-name-temp (gensym "^RULE"))
                (rule-args (cddr exp))
                (old-memo (memo-copy)))
           (debug-pre-apply rule-expr stream store)
           (let ((ans
                  (match rule-expr
                    (`(^ ,name ,from-ometa)
                      (interp/fresh-memo
                       (cons `(,rule-name-temp (apply ,name ,@rule-args))
                             (ometa-eval from-ometa))
                       rule-name-temp stream (fresh-store) *ns*))
                    (rule-name
                     (rule-apply rule-name rule-args stream (fresh-store))))))
             (unless (and (memo rule-expr stream)
                          (m-lr-detected? (memo rule-expr stream)))
               (reset-memo! old-memo))
             (debug-post-apply rule-expr stream store ans)
             (append-old-store ans store))))
        
        ((empty)
         (list :none stream store))
        ((seq)
         (match (e (second exp) stream store)
           ((list val s st) (e (third exp) s st))
           (:failure failure)))
        ((atom)
         (flet ((a? (b) (equal b (cadr exp))))
           (match (e `(apply anything) stream store)
             ((list (guard a (a? a)) s st)
              (list a s st))
             (_ (failure/empty stream store)))))
        ((alt)
         (match (e (second exp) stream store)
           ((list :failure failurelist s t) (e (third exp) stream st))
           (result result)))
        ((many)
         (match (e (second exp) stream store)
           ((list :failure failurelist s st) (list nil stream st))
           (_ (e `(many1 ,(second exp)) stream store))))
        ((many1)
         (match (e (second exp) stream store)
           ((list :failure failurelist s1 st1) (list nil stream st1))
           ((list v1 s1 st1)
            (match (e `(many ,(second exp)) s1 st1)
              ((list v-rest s-rest st-rest)
               (list (append `(,v1) v-rest) s-rest st-rest))))))
        ((~) (match (e (second exp) stream store)
               ((list :failure failurelist s st) (list :none stream st))
               ((list _ s st) (failure #'e stream st nil))))
        ((bind) (match (e (third exp) stream store)
                  ((list val s st)
                   (list val s (cons (list (second exp) val) st)))
                  (failure failure)))
        ((->) (let* ((env (store->env store))
                     (code (second exp))
                     (result (ometa-eval
                              `(let* ,(reverse env)
                                 (declare (ignorable
                                           ,@(mapcar #'first env)))
                                 ,code))))
                (list result stream store)))
        ((->?) (let* ((env (store->env store))
                      (code (second exp))
                      (result (ometa-eval
                               `(let* ,(reverse env)
                                  (declare (ignorable
                                            ,@(mapcar #'first env)))
                                  ,code))))
                 (if result
                     (list :none stream store)
                     (failure/empty stream store))))
        ((list)
         (let* ((temprule (gensym "RULE"))
                (list-pattern (second exp))
                (subprog (cons (list temprule list-pattern) *rules*)))
           (if (empty? stream)
               (failure/empty stream store)
               (match (car stream)
                 ((list pos (guard substream (stream? substream)))
                  (match (interp subprog temprule substream store *ns*)
                    ((list val (guard s (empty? s)) st)
                     (list (de-index-list substream)
                           (cdr stream)
                           st))
                    ((list :failure failurelist s st)
                     (list :failure (cdr failurelist) s st))
                    (substream-is-too-long (failure/empty stream store))))
                 ((list pos (satisfies atom))
                  (failure/empty stream store))
                 (oops (error "Stream cell must contain a value: ~A"
                              (car stream)))))
           )))
    ((list :failure failurelist s st)
     (failure exp stream st failurelist))
    (result
     result)))

(defun interp (omprog start stream store ns)
  (let ((rules omprog)
        (*rules* omprog)
        (*ns* ns))
    (e `(apply ,start) stream store)))

(defun desugar-e (e &optional (i nil))
  (match e
    (`(seq* ,e1) (desugar-e e1 i))
    (`(seq* ,e1 ,e2) `(seq ,(desugar-e e1 i) ,(desugar-e e2 i)))
    (`(seq* ,e1 ,@e2) `(seq ,(desugar-e e1 i)
                            ,(desugar-e `(seq* ,@e2)) i))
    (`(alt* ,e1) (desugar-e e1 i))
    (`(alt* ,e1 ,e2) `(alt ,(desugar-e e1 i) ,(desugar-e e2 i)))
    (`(alt* ,e1 ,@e2) `(alt ,(desugar-e e1 i) ,(desugar-e `(alt* ,@e2) i)))
    (`(many ,e1) `(many ,(desugar-e e1 i)))
    (`(many1 ,e1) `(many1 ,(desugar-e e1 i)))
    (`(many+ ,e1) (let ((a (gensym "+A"))
                        (rest (gensym "+REST"))
                        (body (desugar-e e1 i)))
                    (desugar-e `(seq* (bind ,a ,body)
                                      (bind ,rest (many ,body))
                                      (-> (cons ,a ,rest)))
                               i)))
    (`(apply (^ ,rule-name) ,@args) `(apply (^ ,rule-name ,@i) ,@args))
    (`(bind ,id ,e1) `(bind ,id ,(desugar-e e1 i)))
    (`(~ ,e1) `(~ ,(desugar-e e1 i)))
    (`(list ,e1) `(list ,(desugar-e e1 i)))
    (rest rest)))

(defun desugar-rule (rule i)
  (match rule
    (`(,name ,@ids-and-body)
      `(,name ,@(butlast ids-and-body)
              ,@(desugar-e (last ids-and-body) i)))
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
