(in-package :clometa.c)
(named-readtables:in-readtable :fare-quasiquote)

;;http://www.tinlizzie.org/ometa-js/#OMeta_Compiler

#+nil
(define-layered-method empty
  (exp)
  (list :none *stream* *store*))
#+nil
(define-layered-method seq
  (exp)
  (match (e (second exp))
    ((list val stream store)
     (let ((*stream* stream)
           (*store* store))
       (e (third exp))))
    (failure failure)))
#+nil
(define-layered-method terminal
  (exp)
  (flet ((a? (b) (equal b (cadr exp)))) ;; should this be eql??
    (match (e `(apply anything))
      ((list (guard a (a? a)) stream store)
       (list a stream store))
      (_ (failure/empty)))))
#+nil
(define-layered-method alt
  (exp)
  (match (e (second exp))
    ((list :failure failure-list stream store)
     (let ((*store* store))
       (e (third exp))))
    (result result)))
#+nil
(define-layered-method many
  (exp)
  (match (e (second exp))
    ((list :failure failure-list stream store)
     (list nil *stream* store))
    (_ (e `(many1 ,(second exp))))))
#+nil
(define-layered-method many1
  (exp)
  (match (e (second exp))
    ((list :failure failure-list stream store)
     (list nil *stream* store))
    ((list value stream store)
     (let ((*stream* stream)
           (*store* store))
       (match (e `(many ,(second exp)))
         ((list value-rest stream-rest store-rest)
          (list (append `(,value) value-rest) stream-rest store-rest)))))))
#+nil
(define-layered-method negate
  (exp)
  (match (e (second exp))
    ((list :failure failure-list stream store)
     (list :none *stream* store))
    ((list _ stream store)
     (let ((*store* store))
       (failure #'e nil)))))
#+nil
(define-layered-method bind
  (exp)
  (match (e (third exp))
    ((list value stream store)
     (list value stream (cons (list (second exp) value) store)))
    (failure failure)))
#+nil
(define-layered-method lisp-eval
  (exp)
  (let ((env (store-env))))
  )

(defpackage clometa-memos)

(defun memo-for (symbol)
  (find-symbol (symbol-name symbol) :clometa-memos))

(defmacro defrule (name grammar (&rest args) &body body)
  `(define-layered-method ,name
     :in ,grammar ,args
     (let ((old-memo (memo-copy))
           (result
            (let ((*store* (fresh-store)))
              (labels ((left-recursion? ()
                         (m-lr? (gethash *stream* ,(memo-for name))))
                       (grow-lr (body)
                         (let* ((ans (e body))
                                (ans-stream
                                 (value-stream ans))
                                (memo-entry
                                 (gethash *stream* ,(memo-for name)))
                                (memo-stream
                                 (value-stream (m-value memo-entry))))
                           (if (or (failure? ans)
                                   (>= (length ans-stream)
                                       (length memo-stream)))
                               (m-value memo-entry)
                               (progn
                                 (setf (gethash *stream* ,(memo-for name))
                                       (list ans
                                             (m-lr? memo-entry)
                                             (m-lr-detected? memo-entry)))
                                 (grow-lr body))))))
                (let ((r (reverse
                          (acond
                            ((gethash *stream* ,(memo-for name))
                             (if (left-recursion?)
                                 (align-flags-for-growing-and-failure ',name)
                                 (m-value it)))
                            (t
                             (init-memo/failure-and-align-flags-for-planting-seed name)
                             (let ((*store* (fresh-store)))
                               (let ((ans ,(process-body body))
                                     (m (memo ',name *stream*)))
                                 (memo-add ',name *stream* ans nil (m-lr-detected? m))
                                 (if (and (m-lr-detected? m)
                                          (not (failure? ans)))
                                     (grow-lr it)
                                     ans)))))))))))))
       (unless (and (memo ',name *stream*)
                    (m-lr-detected? (memo ',name *stream*)))
         (reset-memo! old-memo))
       (append-old-store result *store*))))

(defmacro defgrammar (grammar (&optional supergrammar) &rest rules)
  `(progn
     (deflayer ,grammar ,(when supergrammar (list supergrammar)))
     ,@(loop for rule in rules
          collect
            (destructuring-bind (name (&rest args) &body body)
                rule
              ;; predefine in case of mutual recursion
              `(define-layered-method ,name
                 :in ,grammar
                 ,args
                 (declare (ignore ,@args))))
          collect
            `(defparameter ,(intern (symbol-name name) :clometa-memos)
              (make-hash-table :test 'eql)))
     ,@(loop for rule in rules
          collect
            (destructuring-bind (name (&rest args) &body body)
                rule
              `(defrule ,name ,grammar (,@args) ,@body)))))

#+nil
(defgrammar std ()
  (char () (seq* (bind c (anything))
                 (->? (characterp c))
                 (-> c)))
  ;; why bake character-classes into a language when
  ;; its so expressive that adding them is trivial
  (char-range (x y)
              (seq* (bind c (anything))
                    (->? (and (characterp c)
                              (char<= x c y)))
                    (-> c)))
  (letter () (alt* (char-range #\a #\z)
                   (char-range #\A #\Z)))
  (digit () (char-range #\0 #\9))
  (number () (many+ (digit)))
  (spaces () (many+ (atom #\space))))
#+nil
(defgrammar std ()
  (char () (bind c (anything))
        :? (characterp c)
        :-> c)
  ;; why bake character-classes into a language when
  ;; its so expressive that adding them is trivial
  (char-range (x y) (bind c (anything))
              :? (and (characterp c)
                      (char<= x c y))
              :-> c)
  (letter () (or (char-range #\a #\z)
                 (char-range #\A #\Z)))
  (digit () (char-range #\0 #\9))
  (number () (+ (digit)))
  (spaces () (+ (eql #\space))))
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


(defstruct ometa-clause
  bindings
  form)

(define-ometa ometa-grammar
  (symbol (seq* (bind s (anything))
                (->? (symbolp s))
                (-> s)))
  (start (seq* (alt (bind derp (real-clause))
               (list (bind derp (many (real-clause)))))
               (-> (list derp))))
  (real-clause
   (alt*
    (@empty)
    (@eql)
    (@or)
    (@many)
    (@neg)
    (@bind)
    (@->)
    (@->?)
    (@list)
    ;;(empty)
    )
   )
  (@empty (seq* (list (seq* (bind e (symbol))
                            (->? (eql e 'empty))))))
  (@eql (seq* (list (seq* (bind e (symbol))
                          (->? (eql e 'eql))
                          (bind s (symbol))))
              (-> (make-ometa-clause
                   :bindings nil
                   :form `(eql ,s)))))
  (@or (seq* (list (seq* (bind o (symbol))
                         (->? (eql o 'or))
                         (bind clauses
                           (many (real-clause)))))
             (-> (make-ometa-clause
                  :bindings (loop for clause in clauses
                               appending
                                 (ometa-clause-bindings clause))
                  :form `(or ,@(mapcar #'ometa-clause-form clauses))))))
  (@many (seq* (list (seq* (bind m (symbol))
                           (->? (eql m '*))
                           (bind form (real-clause))))
               (-> (make-ometa-clause
                    :bindings nil
                    :form `(* ,(ometa-clause-form form))))))
  (@neg (seq* (list (seq* (bind n (symbol))
                          (->? (eql n '~))
                          (bind form (real-clause))))
              (-> (make-ometa-clause
                   :bindings nil
                   :form `(~ ,form)))))
  (@bind (seq* (list (seq* (bind b (symbol))
                           (->? (eql b 'bind))
                           (bind var (symbol))
                           (bind form (real-clause))))
               (-> (make-ometa-clause
                    :bindings (cons var
                                    (ometa-clause-bindings form))
                    :form `(bind ,var ,(ometa-clause-form form))))))
  (@-> (seq* (list (seq* (bind l (symbol))
                         (->? (eql l '->))
                         (bind form (anything))))
             (-> (make-ometa-clause
                  :bindings nil
                  :form `(-> ,form)))))
  (@->? (seq* (list (seq* (bind p (symbol))
                          (->? (eql p '->?))
                          (bind form (anything))))
              (-> (make-ometa-clause
                   :bindings nil
                   :form `(->? ,form)))))
  (@list (seq* (list (seq* (bind l (symbol))
                           (->? (eql l 'list))
                           (bind form (anything))))
               (-> (make-ometa-clause
                    :bindingl nil
                    :form `(list ,form))))))

#+nil
(first
 (omatch ometa-grammar start
         '((or (* (bind x (eql :x))) (bind y (eql :y)))
           (bind a (eql :a)))))

(defun process-body (body)
  (match body
    (`(:-> ,retval ,@more)
      (when more (error "Additional forms after return: ~A" more))
      retval)
    (`(:? ,pred ,@more)
      `(when ,pred ,(process-body more)))
    (`(,(guard s (symbolp s)) ,@more)
      (process-clause body))))

(defun process-clause (clause)
  (match clause
    (`(anything)
      `(let ((*store* (fresh-store)))
         (anything)))
    (`(eql ,thing)
      `(match (anything)
         ((list (guard a (eql a ,thing)) stream store)
          (list a stream store))
         (_ (failure/empty))))
    (`(or ,@things)
      (if (null things)
          `(failure/empty)
          `(match ,(process-clause (first things))
             ((list :failure failure-list stream store)
              ,(process-clause
                `(or ,@(rest things)))))))
    (`((,@stuff))
      `(progn
         ,@(mapcar #'process-clause stuff)))))
