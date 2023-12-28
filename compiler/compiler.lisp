(require "compiler/insn/arith.lisp")
(require "compiler/insn/control.lisp")
(require "compiler/insn/expr.lisp")
(require "compiler/insn/optimizer.lisp")
(require "compiler/utils/label.lisp")
(require "compiler/utils/optimizer.lisp")

(defun comp(expr)
  (cond
    ((atom expr) (comp-const expr))  ; Compilation des constantes
    ((stringp expr) (comp-var expr)) ; Compilation des variables
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
       ((equal (first expr) 'defun) (comp-defun expr))
       ((symbolp (first expr)) (comp-call expr))
       (t (format t "Expression impossible à compiler: ~A~%" (first expr))) ; Gestion des erreurs
     )
     )
  )
)


(defun optimize-expr (expr)
  (cond
    ;; Base case: If it's an atom, return it as-is
    ((atom expr) expr)

    ;; Simplify comparisons
    ((member (car expr) '(< <= = /= > >=)) (optim-comp expr) )   

    ;; Simplify arithmetic operations
    ((member (car expr) '(+ - * /)) (optim-arith expr))  

    ;; Simplify 'if' expressions
    ((equal (car expr) 'if) (optim-if expr))
    
    ;; Default case: Optimize all sub-expressions (applies a given function (here, #'optimize-expr) to each element of a list and returns a new list of the results.)
    (t (mapcar #'optimize-expr expr))
  )
)  
      
