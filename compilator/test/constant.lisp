(require "compilator/compilator.lisp")
(require "vm/vm.lisp")

(let ((vm '()) (program '10))
  (vm-init vm)
  (vm-load vm (comp program))
  (vm-execute vm)
  (format t "Expression constante: ~A" (= (attr-get vm :R0) 10))
)