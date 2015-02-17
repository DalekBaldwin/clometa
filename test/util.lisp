(in-package :clometa.c-test)

(defmacro seq<< (&rest stuff)
  (let ((gensyms (loop for thing in stuff collect (gensym))))
    `(seq
      ,@(loop for thing in stuff
             for sym in gensyms
             collect `(bind ,sym ,thing))
      :-> (list ,@gensyms))))

(defmacro list<< (&rest clauses)
  (let ((gensyms (loop for clause in clauses collect (gensym))))
    `(seq (list
           ,@(loop for clause in clauses
                for sym in gensyms
                collect `(bind ,sym ,clause)))
          :-> (list ,@gensyms))))
