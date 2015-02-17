(in-package :clometa.c-test)

(in-suite test-all)

;;; lambda-list::= (var*
;;;                [&optional {var | (var [init-form [supplied-p-parameter]])}*]
;;;                [&rest var]
;;;                [&key {var | ({var | (keyword-name var)}
;;;                               [init-form [supplied-p-parameter]])}*
;;;                   [&allow-other-keys]]
;;;                [&aux {var | (var [init-form])}*])

(defparameter *lambda-list-keywords*
  (list '&ALLOW-OTHER-KEYS '&AUX '&BODY '&ENVIRONMENT '&KEY '&OPTIONAL '&REST '&WHOLE))

(defgrammar ordinary-lambda-list ()
  (sat (pred)
       (bind s _)
       :->? (funcall pred s)
       :-> s)
  (var () (sat (lambda (s) (and (symbolp s)
                                (not (keywordp s))
                                (not (member s *lambda-list-keywords*))))))
  (init-form () _)
  (supplied-p-parameter () (sat #'symbolp))
  (start ()
         (seq<< (req-vars)
                (opt-vars)
                (rest-var)
                (key-vars)
                (aux-vars)))
  (req-vars () (* (var)))
  (opt-vars () (? '&optional
                  (* (or (var)
                         (list (var)
                               (? (init-form)
                                  (? (supplied-p-parameter))))))))
  (rest-var () (? '&rest (var)))
  (key-vars () (? '&key
                  (bind vars
                    (* (or (var)
                           (list (or (var)
                                     (list (sat #'keywordp) (var)))
                                 (? (init-form)
                                    (? (supplied-p-parameter)))))))
                  (? '&allow-other-keys)
                  :-> vars))
  (aux-vars () (? '&aux
                  (* (or (var)
                         (list (var) (? (init-form))))))))

(deftest test-ordinary-lambda-list ()
  (is (null
       (nth-value 1
                  (gomatch ordinary-lambda-list
                           start ()
                           '(a b
                             &optional c (d :d) (e :e e-p)
                             &rest rest
                             &key f (g) ((:h h)) (i :i i-p)
                             &aux j (k) (l :l)))))))

;;; lambda-list::= (var*
;;;                 [&optional {var | (var)}*]
;;;                 [&rest var]
;;;                 [&key {var | ({var | (keyword-name var)})}* [&allow-other-keys]])

(defgrammar generic-function-lambda-list ()
  (var () (foreign ordinary-lambda-list var))
  (keyword-name () (foreign ordinary-lambda-list sat #'keywordp))
  (start ()
         (seq<< (foreign ordinary-lambda-list req-vars)
                (opt-vars)
                (foreign ordinary-lambda-list rest-var)
                (key-vars)))
  (opt-vars () (? '&optional
                  (* (or (var)
                         (list (var))))))
  (key-vars () (? '&key
                  (* (or (var)
                         (list (or (var)
                                   (list (keyword-name) (var))))))
                  (? '&allow-other-keys))))

(deftest test-generic-function-lambda-list ()
  (is (null
       (nth-value 1
                  (gomatch generic-function-lambda-list
                           start ()
                           '(a b
                             &optional c (d)
                             &rest rest
                             &key e (f) ((:g g))))))))

;;; lambda-list::= ({var | (var [specializer])}*
;;;                 [&optional {var | (var [init-form [supplied-p-parameter]])}*]
;;;                 [&rest var]
;;;                 [&key {var | ({var | (keyword-name var)}
;;;                                [init-form [supplied-p-parameter]])}*
;;;                    [&allow-other-keys]]
;;;                 [&aux {var | (var [init-form])}*])

(defgrammar specialized-lambda-list (ordinary-lambda-list)
  (specializer () _)
  (req-vars () (* (or (var)
                      (list (var)
                            (? (specializer)))))))

(deftest test-specialized-lambda-list ()
  (is (null
       (nth-value 1
                  (gomatch specialized-lambda-list
                           start ()
                           '(a (b b-class) (c (eql :c))
                             &optional d (e :e) (f :f f-p)
                             &rest rest
                             &key g (h) ((:i i)) (j :j j-p) ((:k k) :k k-p)
                             &aux l (m) (n :n)))))))

;;; reqvars::= var*
;;; optvars::= [&optional {var | (var [init-form [supplied-p-parameter]])}*]
;;; restvar::= [{&rest | &body} var]
;;; keyvars::= [&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}*
;;;             [&allow-other-keys]]
;;; auxvars::= [&aux {var | (var [init-form])}*]
;;; envvar::= [&environment var]
;;; wholevar::= [&whole var]
;;; lambda-list::= (wholevar reqvars optvars restvar keyvars auxvars) |
;;;                (wholevar reqvars optvars . var)

(defgrammar destructuring-lambda-list ()
  (var ()
       (or (foreign ordinary-lambda-list var)
           (list<< (start))))
  (keyword-name ()
                (foreign ordinary-lambda-list sat #'keywordp))
  (init-form ()
             (foreign ordinary-lambda-list init-form))
  (supplied-p-parameter ()
                        (foreign ordinary-lambda-list supplied-p-parameter))
  (start ()
         (or
          (seq<< (whole-var)
                 (req-vars)
                 (opt-vars)
                 (rest-var)
                 (key-vars)
                 (aux-vars))
          #+nil ;; to-do: handle dotted lists
          (list* (req-vars)
                 (opt-vars)
                 (var))))
  (req-vars () (* (var)))
  (opt-vars () (? '&optional
                  (* (or (var)
                         (list (var)
                               (? (init-form)
                                  (? (supplied-p-parameter))))))))
  (rest-var () (? (or '&rest '&body) (var)))
  (key-vars () (? '&key (* (or (var)
                               (list (or (var)
                                         (list (keyword-name) (var)))
                                     (? (init-form)
                                        (? (supplied-p-parameter))))))))
  (aux-vars () (? '&aux (* (or (var)
                               (list (var)
                                     (? (init-form)))))))
  (env-var () (? '&environment (var)))
  (whole-var () (? '&whole (var))))

(deftest test-destructuring-lambda-list ()
  (is (null
       (nth-value 1
                  (gomatch destructuring-lambda-list
                           start ()
                           '(&whole whole
                             a b
                             &optional c (d) (e :e) (f :f f-p)
                             &rest rest
                             &key g (h) ((:i i)) (j :j)
                             &aux k (l) (m :m))))))
  (is (null
       (nth-value 1
                  (gomatch destructuring-lambda-list
                           start ()
                           '(&whole whole
                             (a &optional b)))))))

;;; reqvars::= var*
;;; optvars::= [&optional {var | (var [init-form [supplied-p-parameter]])}*]
;;; restvar::= [{&rest | &body} var]
;;; keyvars::= [&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}*
;;;             [&allow-other-keys]]
;;; auxvars::= [&aux {var | (var [init-form])}*]
;;; envvar::= [&environment var]
;;; wholevar::= [&whole var]
;;; lambda-list::= (wholevar envvar  reqvars envvar  optvars envvar
;;;                 restvar envvar  keyvars envvar  auxvars envvar) |
;;;                (wholevar envvar  reqvars envvar  optvars envvar .  var)
;;; pattern::= (wholevar reqvars optvars restvar keyvars auxvars) |
;;;            (wholevar reqvars optvars . var)

(defparameter *env-var-seen* nil)

(defgrammar macro-lambda-list (destructuring-lambda-list)
  (env-var ()
           ;; &environment can only appear at the top level of a macro lambda
           ;; list, and can only appear once, but can appear anywhere in that list
           (or (seq :->? *env-var-seen*
                    (~ (next-rule)))
               (seq :->? (null *env-var-seen*)
                    (? (bind result (next-rule))
                       :-> (setf *env-var-seen* t)
                       :-> result))))
  (start () (or (list<< (whole-var) (env-var)
                        (req-vars) (env-var)
                        (opt-vars) (env-var)
                        (rest-var) (env-var)
                        (key-vars) (env-var)
                        (aux-vars) (env-var))
                #+nil ;; to-do: handle dotted lists
                (list* (whole-var) (env-var)
                       (req-vars) (env-var)
                       (opt-vars) (env-var) . (var)))))

;;; lambda-list::= (var*
;;;                 [&optional {var | (var [init-form [supplied-p-parameter]])}*]
;;;                 [&rest var]
;;;                 [&key {var | ({var | (keyword-name var)}
;;;                                [init-form [supplied-p-parameter]])}*
;;;                    [&allow-other-keys]]
;;;                 [&environment var]

(defgrammar defsetf-lambda-list (ordinary-lambda-list)
  (env-var () (? '&environment (var)))
  (start ()
         (seq<< (req-vars)
                (opt-vars)
                (rest-var)
                (key-vars)
                (env-var))))
