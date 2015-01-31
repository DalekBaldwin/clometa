(in-package :clometa.i)
(named-readtables:in-readtable :fare-quasiquote)

(defparameter *stream* nil)
(defparameter *store* nil)
(defparameter *rules* nil)
(defparameter *ns* nil)

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

#+nil
(defun interp/fresh-memo (omprog start-rule stream &optional store (ns *ns*))
  (fresh-memo!)
  (interp omprog start-rule stream store ns))

;; special
(defun interp/fresh-memo (omprog start-rule stream)
  (fresh-memo!)
  (let ((*stream* stream))
    (interp omprog start-rule)))

#+nil
(defun failure (e stream store failurelist)
  (cond
    ((empty? stream)
     (list :failure (cons (list e :end :_) failurelist) stream store))
    (t (list :failure (cons (cons e (car stream)) failurelist) stream store))))

;; special
(defun failure (e failurelist)
  (cond
    ((empty? *stream*)
     (list :failure (cons (list e :end :_) failurelist) *stream* *store*))
    (t (list :failure (cons (cons e (car *stream*)) failurelist) *stream* *store*))))

#+nil
(defun failure/empty (stream store)
  (list :failure nil stream store))

;; special
(defun failure/empty ()
  (list :failure nil *stream* *store*))

#+nil
(defun anything (stream store)
  (if (empty? stream)
      (failure/empty stream store)
      (list (de-index-list (cadr (car stream))) (cdr stream) store)))

;; special
(defun anything ()
  (if (empty? *stream*)
      (failure/empty)
      (list (de-index-list (cadr (car *stream*))) (cdr *stream*) *store*)))

#+nil
(defun init-memo/failure-and-align-flags-for-planting-seed (name stream)
  (memo-add name stream (failure/empty stream (fresh-store)) t))

;; special
(defun init-memo/failure-and-align-flags-for-planting-seed (name)
  (memo-add name *stream*
            (let ((*store* (fresh-store)))
              (failure/empty))
            t))

#+nil
(defun align-flags-for-growing-and-failure (name stream store)
  (memo-add name stream (m-value (memo name stream)) nil t)
  (failure/empty stream store))

;; special
(defun align-flags-for-growing-and-failure (name)
  (memo-add name *stream* (m-value (memo name *stream*)) nil t)
  (failure/empty))

#+nil
(defun rule-apply (name args stream store)
  (labels ((left-recursion? ()
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

;; special
(defun rule-apply (name args)
  (labels ((left-recursion? ()
             (m-lr? (memo name *stream*)))
           (grow-lr (body)
             (let* ((ans (e body))
                    (ans-stream (value-stream ans))
                    (memo-entry (memo name *stream*))
                    (memo-stream (value-stream (m-value memo-entry))))
               (if (or (failure? ans)
                       (>= (length ans-stream) (length memo-stream)))
                   (m-value memo-entry)
                   (progn
                     (memo-add name *stream* ans (m-lr? memo-entry)
                               (m-lr-detected? memo-entry))
                     (grow-lr body))))))
    (let ((r (reverse
              (acond
                ((eql name 'anything)
                 (let ((*store* (fresh-store)))
                   (anything)))
                ((memo name *stream*)
                 (if (left-recursion?)
                     (align-flags-for-growing-and-failure name)
                     (m-value it)))
                ((find-rule-by-name name *rules* args)
                 (init-memo/failure-and-align-flags-for-planting-seed name)
                 (let ((*store* (fresh-store)))
                   (let ((ans (e it))
                         (m (memo name *stream*)))
                     (memo-add name *stream* ans nil (m-lr-detected? m))
                     (if (and (m-lr-detected? m)
                              (not (failure? ans)))
                         (grow-lr it)
                         ans))))
                (t (error "no such rule ~A" name))))))
      (reverse (cons (append (car r) *store*) (cdr r))))))

(defmethod ometa-eval (form)
  (eval form))

#+nil
(defun e (exp stream store)
  (match (dispatch (car exp) exp stream store)
    ((list :failure failurelist s st)
     (failure exp stream st failurelist))
    (result
     result)))

;; special
(defun e (exp)
  (match (dispatch (car exp) exp)
    ((list :failure failurelist s st)
     (failure exp failurelist))
    (result
     result)))

#+nil
(defgeneric dispatch (head exp stream store)
  (:method ((head (eql 'apply)) exp stream store)
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
                        from-ometa
                        ;;(ometa-eval from-ometa)
                        )
                  rule-name-temp stream (fresh-store) *ns*))
               (rule-name
                (rule-apply rule-name rule-args stream (fresh-store))))))
        (unless (and (memo rule-expr stream)
                     (m-lr-detected? (memo rule-expr stream)))
          (reset-memo! old-memo))
        (debug-post-apply rule-expr stream store ans)
        (append-old-store ans store))))
  (:method ((head (eql 'empty)) exp stream store)
    (list :none stream store))
  (:method ((head (eql 'seq)) exp stream store)
    (match (e (second exp) stream store)
      ((list val s st) (e (third exp) s st))
      (failure failure)))
  (:method ((head (eql 'atom)) exp stream store)
    (flet ((a? (b) (equal b (cadr exp)))) ;; should this be eql??
      (match (e `(apply anything) stream store)
        ((list (guard a (a? a)) s st)
         (list a s st))
        (_ (failure/empty stream store)))))
  (:method ((head (eql 'alt)) exp stream store)
    (match (e (second exp) stream store)
      ((list :failure failurelist s st)
       (e (third exp) stream st))
      (result result)))
  (:method ((head (eql 'many)) exp stream store)
    (match (e (second exp) stream store)
      ((list :failure failurelist s st) (list nil stream st))
      (_ (e `(many1 ,(second exp)) stream store))))
  (:method ((head (eql 'many1)) exp stream store)
    (match (e (second exp) stream store)
      ((list :failure failurelist s1 st1) (list nil stream st1))
      ((list v1 s1 st1)
       (match (e `(many ,(second exp)) s1 st1)
         ((list v-rest s-rest st-rest)
          (list (append `(,v1) v-rest) s-rest st-rest))))))
  (:method ((head (eql '~)) exp stream store)
    (match (e (second exp) stream store)
      ((list :failure failurelist s st) (list :none stream st))
      ((list _ s st) (failure #'e stream st nil))))
  (:method ((head (eql 'bind)) exp stream store)
    (match (e (third exp) stream store)
      ((list val s st)
       (list val s (cons (list (second exp) val) st)))
      (failure failure)))
  (:method ((head (eql '->)) exp stream store)
    (let* ((env (store->env store))
           (code (second exp))
           (result (ometa-eval
                    `(let* ,(reverse env)
                       (declare (ignorable
                                 ,@(mapcar #'first env)))
                       ,code))))
      (list result stream store)))
  (:method ((head (eql '->?)) exp stream store)
    (let* ((env (store->env store))
           (code (second exp))
           (result (ometa-eval
                    `(let* ,(reverse env)
                       (declare (ignorable
                                 ,@(mapcar #'first env)))
                       ,code))))
      (if result
          (list :none stream store)
          (failure/empty stream store))))
  (:method ((head (eql 'list)) exp stream store)
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
                         (car stream))))))))

;;; special variable version
(defgeneric dispatch (head exp)
  ;; new -- don't need apply
  (:method ((head symbol) exp)
    (let ((old-memo (memo-copy)))
      (let ((ans
             (let ((*store* (fresh-store)))
               (rule-apply head (cdr exp)))))
        (unless (and (memo head *stream*)
                     (m-lr-detected? (memo head *stream*)))
          (reset-memo! old-memo))
        (append-old-store ans *store*))))
  (:method ((head (eql 'foreign)) exp)
    (let* ((rule-expr (cadr exp))
           (rule-name-temp (gensym "^RULE"))
           (rule-args (cddr exp))
           (old-memo (memo-copy)))
      (let ((ans
             (match rule-expr
               (`(^ ,name ,from-ometa)
                 (let ((*store* (fresh-store)))
                   (interp/fresh-memo
                    (cons `(,rule-name-temp (,name ,@rule-args))
                          from-ometa)
                    rule-name-temp
                    *stream*))))))
        (unless (and (memo exp *stream*)
                     (m-lr-detected? (memo exp *stream*)))
          (reset-memo! old-memo))
        (append-old-store ans *store*))))
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
    (list :none *stream* *store*))
  (:method ((head (eql 'seq)) exp)
    (match (e (second exp))
      ((list val s st)
       (let ((*stream* s)
             (*store* st))
         (e (third exp))))
      (failure failure)))
  (:method ((head (eql 'atom)) exp)
    (flet ((a? (b) (equal b (cadr exp)))) ;; should this be eql??
      (match (e `(anything))
        ((list (guard a (a? a)) s st)
         (list a s st))
        (_ (failure/empty)))))
  (:method ((head (eql 'alt)) exp)
    (match (e (second exp))
      ((list :failure failurelist s st)
       (let ((*store* st))
         (e (third exp))))
      (result result)))
  (:method ((head (eql 'many)) exp)
    (match (e (second exp))
      ((list :failure failurelist s st) (list nil *stream* st))
      (_ (e `(many1 ,(second exp))))))
  (:method ((head (eql 'many1)) exp)
    (match (e (second exp))
      ((list :failure failurelist s1 st1) (list nil *stream* st1))
      ((list v1 s1 st1)
       (let ((*stream* s1)
             (*store* st1))
         (match (e `(many ,(second exp)))
           ((list v-rest s-rest st-rest)
            (list (append `(,v1) v-rest) s-rest st-rest)))))))
  (:method ((head (eql '~)) exp)
    (match (e (second exp))
      ((list :failure failurelist s st) (list :none *stream* st))
      ((list _ s st)
       (let ((*store* st))
         (failure #'e nil)))))
  (:method ((head (eql 'bind)) exp)
    (match (e (third exp))
      ((list val s st)
       (list val s (cons (list (second exp) val) st)))
      (failure failure)))
  (:method ((head (eql '->)) exp)
    (let* ((env (store->env *store*))
           (code (second exp))
           (result (ometa-eval
                    `(let* ,(reverse env)
                       (declare (ignorable
                                 ,@(mapcar #'first env)))
                       ,code))))
      (list result *stream* *store*)))
  (:method ((head (eql '->?)) exp)
    (let* ((env (store->env *store*))
           (code (second exp))
           (result (ometa-eval
                    `(let* ,(reverse env)
                       (declare (ignorable
                                 ,@(mapcar #'first env)))
                       ,code))))
      (if result
          (list :none *stream* *store*)
          (failure/empty))))
  (:method ((head (eql 'list)) exp)
    (let* ((temprule (gensym "RULE"))
           (list-pattern (second exp))
           (subprog (cons (list temprule list-pattern) *rules*)))
      (if (empty? *stream*)
          (failure/empty)
          (match (car *stream*)
            ((list pos (guard substream (stream? substream)))
             (match (let ((*stream* substream))
                      (interp subprog temprule))
               ((list val (guard s (empty? s)) st)
                (list (de-index-list substream)
                      (cdr *stream*)
                      st))
               ((list :failure failurelist s st)
                (list :failure (cdr failurelist) s st))
               (substream-is-too-long (failure/empty))))
            ((list pos (satisfies atom))
             (failure/empty))
            (oops (error "Stream cell must contain a value: ~A"
                         (car *stream*))))))))

#+nil
(defun interp (omprog start stream store ns)
  (let ((rules omprog)
        (*rules* omprog)
        (*ns* ns))
    (e `(apply ,start) stream store)))

;; special
(defun interp (omprog start)
  (let ((rules omprog)
        (*rules* omprog))
    (e `(,start))))


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
