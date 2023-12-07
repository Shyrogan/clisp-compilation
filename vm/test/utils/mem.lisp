(require "vm/utils/mem.lisp")

(format t "Test IS-CONST: ~A~%" (is-const '(:CONST 4)))
(format t "Test IS-VM-ATTR: ~A~%" (is-vm-attr "R0"))
