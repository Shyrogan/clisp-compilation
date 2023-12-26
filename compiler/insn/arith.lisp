(defun comp-add (operands)
  (let ((op1 (first operands))
        (op2 (second operands)))
    (if (and (atom op1) (atom op2))
        (let ((result (+ op1 op2)))
          `((MOVE (:CONST ,result) R0) (PUSH R0)))
        (append (comp op1)
                (comp op2)
                '((POP R1) (POP R0) (ADD R1 R0) (PUSH R0))))))

(defun comp-sub (operands)
  (let ((op1 (first operands))
        (op2 (second operands)))
    (if (and (atom op1) (atom op2))
        (let ((result (- op1 op2)))
          `((MOVE (:CONST ,result) R0) (PUSH R0)))
        (append (comp op1)
                (comp op2)
                '((POP R1) (POP R0) (SUB R1 R0) (PUSH R0))))))

(defun comp-mul(operands)
  (let ((op1 (first operands))
        (op2 (second operands)))
    (if (and (atom op1) (atom op2))
        (let ((result (* op1 op2)))
          `((MOVE (:CONST ,result) R0) (PUSH R0)))
        (append (comp op1)
                (comp op2)
                '((POP R1) (POP R0) (MUL R1 R0) (PUSH R0))))))

(defun comp-div(operands)
  (let ((op1 (first operands))
        (op2 (second operands)))
    (if (and (atom op1) (atom op2))
        (let ((result (/ op1 op2)))
          `((MOVE (:CONST ,result) R0) (PUSH R0)))
        (append (comp op1)
                (comp op2)
                '((POP R1) (POP R0) (DIV R1 R0) (PUSH R0))))))

(defun comp-ge(operands)
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
     `((JLE ,etiq-true)
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
     `((JLT ,etiq-true)
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
     `((JGT ,etiq-true)
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
     `((JEQ ,etiq-true)
       (MOVE (:CONST 0) R0)
       (JMP ,etiq-end)
       (LABEL ,etiq-true)
       (MOVE (:CONST 1) R0)
       (LABEL ,etiq-end)))))