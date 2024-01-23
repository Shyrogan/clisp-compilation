(defun booleanp (value)
  (or (eq value t) (eq value nil)))
  
(defun contains-false (lst)
  (member nil lst))

(defun contains-true (lst)
  (member t lst))