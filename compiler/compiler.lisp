(require "compiler/insn/arith.lisp")
(require "compiler/insn/expr.lisp")

(defun comp(expr)
  (cond
    ((atom expr) (comp-const expr))  ; Soit l'expression est atomique (constante)
    ((stringp expr) (comp-var expr)) ; ou une variable à la limite
    ((listp expr) (cond              ; Cas où l'expression est une liste (en gros, les expressions de la forme (expr ...))
        ((equal (first expr) '+) (comp-add (cdr expr))) ; (+ X X)
        ((equal (first expr) '-) (comp-sub (cdr expr))) ; (- X X)
        ((equal (first expr) '/) (comp-div (cdr expr))) ; (/ X X)
        ((equal (first expr) '*) (comp-mul (cdr expr))) ; (* X X)
    ))
    (format t "Expression impossible à compiler: ~A~%" expr)
))