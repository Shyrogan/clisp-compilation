(require "compiler/insn/arith.lisp")
(require "compiler/insn/control.lisp")
(require "compiler/insn/expr.lisp")
(require "compiler/utils/label.lisp")

(defun comp(expr)
  (let ((ctx '())) (cond
    ((symbolp expr) (comp-var expr)) ; Compilation des variables
    ((atom expr) (comp-const expr))  ; Compilation des constantes
    ((listp expr)                    ; Compilation des expressions de liste
     (cond
       ((equal (first expr) '+) (comp-add (cdr expr)))
       ((equal (first expr) '-) (comp-sub (cdr expr)))
       ((equal (first expr) '/) (comp-div (cdr expr)))
       ((equal (first expr) '*) (comp-mul (cdr expr)))
       ((equal (first expr) '>=) (comp-ge (cdr expr)))
       ((equal (first expr) '<=) (comp-le (cdr expr)))
       ((equal (first expr) '=) (comp-eq (cdr expr)))
       ((equal (first expr) '>) (comp-gt (cdr expr)))
       ((equal (first expr) '<) (comp-lt (cdr expr)))
       ((equal (first expr) 'progn) (comp-seq (cdr expr)))
       ((equal (first expr) 'if) (comp-if (cdr expr)))
       ((equal (first expr) 'while) (comp-while (cdr expr)))
       ((equal (first expr) 'for) (comp-for (cdr expr)))
       ((equal (first expr) 'setf) (comp-setf (cdr expr)))
      ;;  ((equal (first expr) 'defun) (comp-defun expr))
      ;;  ((symbolp (first expr)) (comp-call expr))
       (t (format t "Expression impossible à compiler: ~A~%" (first expr))) ; Gestion des erreurs
     ))
  ))
)