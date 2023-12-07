(require "vm/vm.lisp")

;; Vérifie que le programme s'arrête
(let ((vm '()))
  (vm-init vm)
  (vm-load vm '(
    (MOVE (:CONST 10) R0)
    (HALT)
    (INCR R0)
  ))
  (vm-execute vm)
  (format t "Test HALT CONST: ~A~%" (= (attr-get vm :R0) 10)))