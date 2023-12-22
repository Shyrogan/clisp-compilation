(defun comp-const(expr)
  (list (list 'MOVE (list :CONST expr) 'R0)
        (list 'PUSH 'R0)))

(defun comp-var(expr)
  (list (list 'MOVE (list :CONST expr) 'R0)
        (list 'PUSH 'R0)))