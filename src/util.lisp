(in-package :clometa)

(defun build-list (length function)
  (loop for i from 0 below length
       collect (funcall function i)))

(defun construct-stream (input)
  (labels ((list->stream (l &optional (depth 0))
             (build-list (length l)
                         (lambda (n)
                           `((,depth ,n)
                             ,(cond
                               ((listp (nth n l))
                                (list->stream (nth n l) n))
                               (t (nth n l)))))))
           (string->stream (s &optional (depth 0))
             (build-list (length s)
                         (lambda (n)
                           `((,depth ,n) ,(elt s n))))))
    (cond
      ((listp input) (list `((-1 0) ,(list->stream input))))
      ((stringp input) (string->stream input)))))

(defun andmap (function list &optional accum)
  (acond
    ((null list)
     accum)
    ((funcall function (first list))
     (andmap function (rest list) it))
    (t
     nil)))

(defun stream? (stream)
  (and (list? stream)
       (andmap (lambda (p)
                 (match p
                   ((list (list (satisfies numberp) (satisfies numberp)) _) t)
                   (rest nil)))
               stream)))

(defun de-index-list (l)
  (flet ((de-index (node)
           (cond
             ((list? (cadr node)) (de-index-list (cadr node)))
             (t (cadr node)))))
    (if (list? l)
        (mapcar #'de-index l)
        l)))

(defparameter *table* (make-hash-table :test #'equal))
(defun fresh-memo! ()
  (setf *table* (make-hash-table)))
(defun reset-memo! (to)
  (setf *table* to))
(defun memo-copy ()
    (copy-hash-table *table*))
(defun memo (rule-name stream)
  (gethash (list rule-name stream) *table*))
(defun memo-add (rule-name stream value &optional (lr? nil) (lr-detected? nil))
  (setf (gethash (list rule-name stream) table) (list value lr? lr-detected?)))
(defun fresh-store () (lambda ()))
(defun store->env (a-list)
  (flet ((quote-value (binding)
           (match binding
             ((list id v) `(,id ',v)))))
    (map #'quote-value a-list)))
(defun append-old-store (ans old-store)
  (let* ((rev-ans (reverse ans))
         (new-store (car rev-ans)))
    (reverse (cons (append old-store new-store) (cdr rev-ans)))))
;;(defun pprint (result))
(defun ptable (stuff)
  ;;(format t "~&~A~A    Memo:~A")
  (maphash (lambda (k v)
             (format t "~A ~A ==> ~A ~A~A"
                     (first k) (stream-pos0 (second k))
                     (first (m-value v)) (stream-pos0 (value-stream (m-value v)))))))

(defun fail? (v) (equal :fail (car v)))
(defun success? (v) (not (fail? v)))
(defun value-stream (v)
  (match v
    ((list :fail _ s _) s)
    ((list val s _) s)))
(defun stream-pos0 (s)
  (if (empty? s)
      nil
      (caar s)))
(setf (symbol-function 'm-lr?)  #'second)
(setf (symbol-function 'm-lr-detected?) #'third)

(defparameter *debug* nil)
(defparameter *debug-count* 0)

(defun debug-pre-apply (rule-name stream store)
  (when *debug*
    (unless (eql rule-name :anything)
      (format t "~&~A   |~A -stream ~A -store ~A~%"
              (list->string (build-list *debug-count*)
                            (lambda (n) #\>))
              rule-name (de-index-list strieam) store)
      (incf *debug-count*))))

(defun debug-post-apply (rule-name stream store ans)
  (when *debug*
    (unless (eql rule-name :anything)
      (format t "~&~A   |~A -stream ~A -store ~A~%"
              (list->string (build-list *debug-count*)
                            (lambda (n) #\<))
              rule-name (de-index-list strieam)
              (append store (last ans)) (first ans)))))
