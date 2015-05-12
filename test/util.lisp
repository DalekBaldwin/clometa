(in-package :clometa.c-test)

(defun @-clause-p (clause)
  (and (listp clause)
       (eql (first clause) '@)))

(defmacro seq<< (&rest clauses)
  "Collect the result of each clause in a seq form into a list."
  (let ((gensyms (loop for clause in clauses collect (gensym))))
    `(seq
      ,@(loop for clause in clauses
           for sym in gensyms
           collect `(bind ,sym ,(if (@-clause-p clause)
                                    (second clause)
                                    clause)))
      :-> (append ,@(loop for clause in clauses
                       for sym in gensyms
                       if (@-clause-p clause)
                       collect sym
                       else collect `(list ,sym))))))

(defmacro ?/ (&rest clauses)
  "Consume an optional expression without returning output."
  `(seq (? ,@clauses)
        :-> nil))

(defmacro list<< (&rest clauses)
  "Collect the result of each clause in a list form into a list."
  (let ((gensyms (loop for clause in clauses collect (gensym))))
    `(seq (list
           ,@(loop for clause in clauses
                for sym in gensyms
                collect `(bind ,sym ,(if (@-clause-p clause)
                                         (second clause)
                                         clause))))
          :-> (append ,@(loop for clause in clauses
                           for sym in gensyms
                           if (@-clause-p clause)
                           collect sym
                           else collect `(list ,sym))))))
