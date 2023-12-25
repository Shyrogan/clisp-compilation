(defun comp-add(operands)
  (let ((instructions '()))
    (when operands
      (setq instructions (append instructions (comp (cadr operands)))) ; Compile the second operand
      (setq instructions (append instructions (comp (car operands)))) ; Compile the first operand
      (setq instructions (append instructions '((POP R0) (POP R1) (ADD R1 R0) (PUSH R0))))) ; Perform subtraction
    instructions))

(defun comp-sub(operands)
  (let ((instructions '()))
    (when operands
      (setq instructions (append instructions (comp (cadr operands)))) ; Compile the second operand
      (setq instructions (append instructions (comp (car operands)))) ; Compile the first operand
      (setq instructions (append instructions '((POP R0) (POP R1) (SUB R1 R0) (PUSH R0))))) ; Perform subtraction
    instructions))

(defun comp-mul(operands)
  (let ((instructions '()))
    (when operands
      (setq instructions (append instructions (comp (cadr operands)))) ; Compile the second operand
      (setq instructions (append instructions (comp (car operands)))) ; Compile the first operand
      (setq instructions (append instructions '((POP R0) (POP R1) (MUL R1 R0) (PUSH R0))))) ; Perform subtraction
    instructions))

(defun comp-div(operands)
  (let ((instructions '()))
    (when operands
      (setq instructions (append instructions (comp (cadr operands)))) ; Compile the second operand
      (setq instructions (append instructions (comp (car operands)))) ; Compile the first operand
      (setq instructions (append instructions '((POP R0) (POP R1) (DIV R1 R0) (PUSH R0))))) ; Perform subtraction
    instructions))

(defun comp-ge(operands)
  (let ((etiq-true (generate-label))
        (etiq-end (generate-label)))
    (append
     ;; Compiler les opérandes
     (comp (car operands))
     (comp (cadr operands))
     ;; Effectuer la comparaison
     '((POP R0) (POP R1) (CMP R1 R0))
     ;; Utiliser les sauts conditionnels pour définir R0
     `((JGE ,etiq-true)
       (MOVE (:CONST 0) R0)
       (JMP ,etiq-end)
       (LABEL ,etiq-true)
       (MOVE (:CONST 1) R0)
       (LABEL ,etiq-end))
)))

(defun comp-le (operands)
  (let ((etiq-true (generate-label))
        (etiq-end (generate-label)))
    (append
     ;; Compiler les opérandes
     (comp (car operands))
     (comp (cadr operands))
     ;; Effectuer la comparaison
     '((POP R1) (POP R0) (CMP R0 R1))
     ;; Utiliser les sauts conditionnels pour définir R0
     `((JGE ,etiq-true)
       (MOVE (:CONST 0) R0)
       (JMP ,etiq-end)
       (LABEL ,etiq-true)
       (MOVE (:CONST 1) R0)
       (LABEL ,etiq-end)))))

(defun comp-lt (operands)
  (let ((etiq-true (generate-label))
        (etiq-end (generate-label)))
    (append
     ;; Compiler les opérandes
     (comp (car operands))
     (comp (cadr operands))
     ;; Effectuer la comparaison
     '((POP R1) (POP R0) (CMP R0 R1))
     ;; Utiliser les sauts conditionnels pour définir R0
     `((JGE ,etiq-true)
       (MOVE (:CONST 0) R0)
       (JMP ,etiq-end)
       (LABEL ,etiq-true)
       (MOVE (:CONST 1) R0)
       (LABEL ,etiq-end)))))

(defun comp-gt (operands)
  (let ((etiq-true (generate-label))
        (etiq-end (generate-label)))
    (append
     ;; Compiler les opérandes
     (comp (car operands))
     (comp (cadr operands))
     ;; Effectuer la comparaison
     '((POP R1) (POP R0) (CMP R0 R1))
     ;; Utiliser les sauts conditionnels pour définir R0
     `((JGE ,etiq-true)
       (MOVE (:CONST 0) R0)
       (JMP ,etiq-end)
       (LABEL ,etiq-true)
       (MOVE (:CONST 1) R0)
       (LABEL ,etiq-end)))))

(defun comp-eq (operands)
  (let ((etiq-true (generate-label))
        (etiq-end (generate-label)))
    (append
     ;; Compiler les opérandes
     (comp (car operands))
     (comp (cadr operands))
     ;; Effectuer la comparaison
     '((POP R1) (POP R0) (CMP R0 R1))
     ;; Utiliser les sauts conditionnels pour définir R0
     `((JGE ,etiq-true)
       (MOVE (:CONST 0) R0)
       (JMP ,etiq-end)
       (LABEL ,etiq-true)
       (MOVE (:CONST 1) R0)
       (LABEL ,etiq-end)))))