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