(defun comp-add (operands ctx)
  (append (comp (first operands) ctx)
          (comp (second operands) ctx)
          '((POP R1) (POP R0) (ADD R1 R0) (PUSH R0))))

(defun comp-sub (operands ctx)
  (append (comp (first operands) ctx)
          (comp (second operands) ctx)
          '((POP R1) (POP R0) (SUB R1 R0) (PUSH R0))))

(defun comp-mul (operands ctx)
  (append (comp (first operands) ctx)
          (comp (second operands) ctx)
          '((POP R1) (POP R0) (MUL R1 R0) (PUSH R0))))

(defun comp-div (operands ctx)
  (append (comp (first operands) ctx)
          (comp (second operands) ctx)
          '((POP R1) (POP R0) (DIV R1 R0) (PUSH R0))))

(defun comp-ge(operands ctx)
  (let ((etiq-true (generate-label))
        (etiq-end (generate-label)))
    (append
     ;; Compiler les opérandes
     (comp (car operands) ctx)
     (comp (cadr operands) ctx)
     ;; Effectuer la comparaison
     '((POP R1) (POP R0) (CMP R0 R1))
     ;; Utiliser les sauts conditionnels pour définir R0
     `((JGE ,etiq-true)
       (MOVE (:CONST nil) R0)
       (JMP ,etiq-end)
       (LABEL ,etiq-true)
       (MOVE (:CONST t) R0)
       (LABEL ,etiq-end))
)))

(defun comp-le (operands ctx)
  (let ((etiq-true (generate-label))
        (etiq-end (generate-label)))
    (append
     ;; Compiler les opérandes
     (comp (car operands) ctx)
     (comp (cadr operands) ctx)
     ;; Effectuer la comparaison
     '((POP R1) (POP R0) (CMP R0 R1))
     ;; Utiliser les sauts conditionnels pour définir R0
     `((JLE ,etiq-true)
       (MOVE (:CONST nil) R0)
       (JMP ,etiq-end)
       (LABEL ,etiq-true)
       (MOVE (:CONST t) R0)
       (LABEL ,etiq-end)))))

(defun comp-lt (operands ctx)
  (let ((etiq-true (generate-label))
        (etiq-end (generate-label)))
    (append
     ;; Compiler les opérandes
     (comp (car operands) ctx)
     (comp (cadr operands) ctx)
     ;; Effectuer la comparaison
     '((POP R1) (POP R0) (CMP R0 R1))
     ;; Utiliser les sauts conditionnels pour définir R0
     `((JLT ,etiq-true)
       (MOVE (:CONST nil) R0)
       (JMP ,etiq-end)
       (LABEL ,etiq-true)
       (MOVE (:CONST t) R0)
       (LABEL ,etiq-end)))))

(defun comp-gt (operands ctx)
  (let ((etiq-true (generate-label))
        (etiq-end (generate-label)))
    (append
     ;; Compiler les opérandes
     (comp (car operands) ctx)
     (comp (cadr operands) ctx)
     ;; Effectuer la comparaison
     '((POP R1) (POP R0) (CMP R0 R1))
     ;; Utiliser les sauts conditionnels pour définir R0
     `((JGT ,etiq-true)
       (MOVE (:CONST nil) R0)
       (JMP ,etiq-end)
       (LABEL ,etiq-true)
       (MOVE (:CONST t) R0)
       (LABEL ,etiq-end)))))

(defun comp-eq (operands ctx)
  (let ((etiq-true (generate-label))
        (etiq-end (generate-label)))
    (append
     ;; Compiler les opérandes
     (comp (car operands) ctx)
     (comp (cadr operands) ctx)
     ;; Effectuer la comparaison
     '((POP R1) (POP R0) (CMP R0 R1))
     ;; Utiliser les sauts conditionnels pour définir R0
     `((JEQ ,etiq-true)
       (MOVE (:CONST nil) R0)
       (JMP ,etiq-end)
       (LABEL ,etiq-true)
       (MOVE (:CONST t) R0)
       (LABEL ,etiq-end)))))