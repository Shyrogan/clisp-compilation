(defun optim-comp (expr)
    (let ((operands (mapcar #'optimize-expr (cdr expr))))
          (if (every #'numberp operands)            ;; if the parameter are constants
               (if (apply (car expr) (cdr expr)) t nil) ;;the apply the comparision 
                    (cons (car expr) operands)))  ;; if not just keep it as it is with it's simplified parameters
    )

(defun optim-if (expr)
  (let ((test-part (optimize-expr (second expr)))
        (then-part (optimize-expr (third expr)))
        (else-part (optimize-expr (fourth expr))))
    (if (booleanp test-part)   ;; if the test is boolean   
        (if (eq test-part t) then-part else-part)    ;; the condition can be deleted    
        (list 'if test-part then-part else-part))))     ;;if not we will put the condition with simplified then and else if possible
     

(defun optim-arith (expr)
    (let ((operands (mapcar #'optimize-expr (cdr expr))))
        (if (every #'numberp operands)   ;; if the parameter are constants
            (eval (cons (car expr) operands))   ;; then do the math
            (cons (car expr) operands)))   ;; if not just put it as it is 
    )
       
(defun optim-not (expr)
  (let ((operand (optimize-expr (car (cdr expr)))))
    (cond
      ((eq t operand) nil)
      ((eq nil operand) t)
      (t (list 'not operand))
    )
  )
)



(defun optim-and (expr)
    (let ((operands (mapcar #'optimize-expr (cdr expr))))
        (if (contains-false operands)
            nil
            (if (every #'booleanp operands)
                (eval (cons (car expr) operands))
                (cons (car expr) operands))))
    )
    
(defun optim-or (expr)
    (let ((operands (mapcar #'optimize-expr (cdr expr))))
        (if (contains-true operands)
            t
            (if (every #'booleanp operands)
                (eval (cons (car expr) operands))
                (cons (car expr) operands))))
    )


