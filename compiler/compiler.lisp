(defun comp(expr)
  (cond
    ((atom expr) (comp-const expr))  ; Soit l'expression est atomique (constante)
    ((listp expr) (cond              ; Cas où l'expression est une liste (en gros, les expressions de la forme (expr ...))
        ((equal (first expr) '+) (comp-add (cdr expr))) ; (+ X X)
        ((equal (first expr) '-) (comp-sub (cdr expr))) ; (- X X)
        ((equal (first expr) '/) (comp-div (cdr expr))) ; (/ X X)
        ((equal (first expr) '*) (comp-mul (cdr expr))) ; (* X X)
    ))
    (format t "Expression impossible à compiler: ~A" expr)
))

(defun comp-const(expr)
  (list (list 'MOVE (list :CONST expr) 'R0)
        (list 'PUSH 'R0)))

(defun comp-add(operands)
  (let ((instructions '()))
    (when operands
      (setq instructions (append instructions (comp (cadr operands)))) ; Compile the second operand
      (setq instructions (append instructions (comp (car operands)))) ; Compile the first operand
      (setq instructions (append instructions '((POP R0) (POP R1) (ADD R1 R0))))) ; Perform subtraction
    instructions))

(defun comp-sub(operands)
  (let ((instructions '()))
    (when operands
      (setq instructions (append instructions (comp (cadr operands)))) ; Compile the second operand
      (setq instructions (append instructions (comp (car operands)))) ; Compile the first operand
      (setq instructions (append instructions '((POP R0) (POP R1) (SUB R1 R0))))) ; Perform subtraction
    instructions))

(defun comp-mul(operands)
  (let ((instructions '()))
    (when operands
      (setq instructions (append instructions (comp (cadr operands)))) ; Compile the second operand
      (setq instructions (append instructions (comp (car operands)))) ; Compile the first operand
      (setq instructions (append instructions '((POP R0) (POP R1) (MUL R1 R0))))) ; Perform subtraction
    instructions))

(defun comp-div(operands)
  (let ((instructions '()))
    (when operands
      (setq instructions (append instructions (comp (cadr operands)))) ; Compile the second operand
      (setq instructions (append instructions (comp (car operands)))) ; Compile the first operand
      (setq instructions (append instructions '((POP R0) (POP R1) (DIV R1 R0))))) ; Perform subtraction
    instructions))