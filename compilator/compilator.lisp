(defun comp(expr)
  (cond
    ((atom expr) (comp-const expr))  ; Soit l'expression est atomique (constante)
    ((listp expr) (cond              ; Cas où l'expression est une liste (en gros, les expressions de la forme (expr ...))
        ((equal (first expr) '+) (comp-add (cdr expr))) ; (+ X X)
    ))
    (format t "Expression impossible à compiler: ~A" expr)
))

(defun comp-const(expr)
  (list (list 'MOVE (list :CONST expr) 'R0)
        (list 'PUSH 'R0)))

(defun comp-add (operands)
  (let ((instructions '()))
    (when operands
      (setq instructions (append instructions (comp (car operands)))) ; Compile the first operand
      (dolist (operand (cdr operands))
        (setq instructions (append instructions (comp operand))))
      (setq instructions (append instructions '((POP R0) (POP R1) (ADD R1 R0))))) ; JSR de préférence quoi
    instructions))