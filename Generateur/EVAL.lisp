(defun eval (expr &optional env)
    (cond 
        ((and (atom expr)(constantp expr)) 
        expr)
;;an atom is any object that is not a cons cell. Atoms include numbers, symbols, characters, strings, and the special values NIL and T
        ((atom expr)  ;; atom qui n'est pas un constant c'est une varible !
        (let ((cell (assoc expr env))
            (if cell 
                (cdr cell)
                (error "~s n'est pas une varibale" expr)))
        ))
        
;;a constant might be a literal number, a string, NIL, T, or other literal data that doesn't change.
        ((and (consp ( car expr)) ( eq'lambda(carr expr)));; λ-fonction
        (eval-body (cddar expr) ;;cddar donne le body 
            (make-env (cadar expr) env) ;; cadar donne les parametres
            env))
        
          
;;consp for lists  
        ((or (not (symbolp (car expr))) (constantp (car expr))))
;;A symbol in Lisp is a data type used to represent identifiers, like variable names





        (t (eval expr))
    )
)