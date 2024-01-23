(defun comp-const (expr ctx)
  `((MOVE (:CONST ,expr) :R0) (PUSH :R0)))