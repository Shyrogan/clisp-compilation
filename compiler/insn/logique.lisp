(defun comp-and (expr ctx)
    (let (
        (etiq-false (generate-label))
        (etiq-end (generate-label))
        (first-expr (first expr))
        (second-expr (second expr)))
        (append
           (comp first-expr ctx)
          `((CMP (:CONST 0) R0) (JEQ ,etiq-false)) ;; jump si exp1 est false
           (comp second-expr ctx)
          `((CMP (:CONST 0) R0) (JEQ ,etiq-false)) ;; jump si exp2 est false
          `((MOVE (:CONST 1) R0) (JMP ,etiq-end) (PUSH R0)) ;; les deux sont true donc R0 =1
          `((LABEL ,etiq-false) (MOVE (:CONST 0) R0) (PUSH R0)) ;; Il y a au moins un qui est faux donc R0=0
          `((LABEL ,etiq-end))   
       )
    )
)

(defun comp-or (expr ctx)
    (let (
        (etiq-true (generate-label))
        (etiq-end (generate-label))
        (first-expr (first expr))
        (second-expr (second expr)))
        (append
           (comp first-expr ctx)
          `((CMP (:CONST 1) R0) (JEQ ,etiq-true)) ;; jump si exp1 est true
           (comp second-expr ctx)
          `((CMP (:CONST 0) R0) (JEQ ,etiq-true)) ;; jump si exp2 est true
          `((MOVE (:CONST 0) R0) (JMP ,etiq-end) (PUSH R0)) ;; les deux sont fausse donc R0 =0
          `((LABEL ,etiq-true) (MOVE (:CONST 1) R0) (PUSH R0)) ;; Il y a au moins un qui est true donc R0=1
          `((LABEL ,etiq-end))   
       )
    )
)

(defun comp-not (expr ctx)
    (let (
        (etiq-true (generate-label))
        (etiq-end (generate-label)))
        (append
           (comp expr ctx)
          `((CMP (:CONST 1) R0) (JEQ ,etiq-true)) ;; jump si exp1 est true
          `((MOVE (:CONST 0) R0) (JMP ,etiq-end) (PUSH R0)) ;; retourne 1 si n'était pas true (donc false) et jump end 
          `((LABEL ,etiq-true) (MOVE (:CONST 0) R0) (PUSH R0)) ;; retourne 0 si était true
          `((LABEL ,etiq-end))   
    )
    )

)