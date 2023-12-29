(defun comp-const (expr)
  `((MOVE (:CONST ,expr) R0) (PUSH R0)))

(defun comp-var (expr)
  `((MOVE (:@ ,expr) R0) (PUSH R0)))

(defun comp-setf(operands)
  ;; operands est de la forme (variable valeur)
  (let ((variable (string (first operands)))  ; Le nom de la variable
        (valeur (second operands)))   ; La valeur à assigner
    (append
     (comp valeur)
     `((STORE R0 (:@ ,variable)))))) 
