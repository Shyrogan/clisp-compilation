(require "compiler/insn/arith.lisp")
(require "compiler/insn/control.lisp")
(require "compiler/insn/expr.lisp")
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
    ((member (car expr) '(< <= = /= > >=))
     (let ((operands (mapcar #'optimize-expr (cdr expr))))
          (if (every #'numberp operands)            ;; if the parameter are constants
               (if (apply (car expr) (cdr expr)) t nil) ;;the apply the comparision 
                    (cons (car expr) operands))) )   ;; if not just keep it as it is with it's simplified parameters

    ;; Simplify arithmetic operations
    ((member (car expr) '(+ - * /))
     (let ((operands (mapcar #'optimize-expr (cdr expr))))
       (if (every #'numberp operands)   ;; if the parameter are constants
           (eval (cons (car expr) operands))   ;; then do the math
               (cons (car expr) operands))))   ;; if not just put it as it is 

    ;; Simplify 'if' expressions
    ((equal (car expr) 'if) 
     (let ((test-part (optimize-expr (second expr)))
           (then-part (optimize-expr (third expr)))
           (else-part (optimize-expr (fourth expr))))
           (if ( booleanp test-part )                 ;; if the test is boolean 
           (if (eq test-part t) then-part else-part) ;; the condition can be deleted 
           (list 'if test-part then-part else-part ) ) ) ) ;;if not we will put the condition with simplified then and else if possible
    
    ;; Default case: Optimize all sub-expressions (applies a given function (here, #'optimize-expr) to each element of a list and returns a new list of the results.)
    (t (mapcar #'optimize-expr expr))))  
      
