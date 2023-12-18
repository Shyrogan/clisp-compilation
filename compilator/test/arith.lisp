(require "compilator/compilator.lisp")
(require "vm/vm.lisp")

(let ((vm '()) (program (list '+ 10 15)))
  (vm-init vm)
  (vm-load vm (comp program))
  (vm-execute vm)
  (format t "Addition de deux constantes (10+15) ~A (insn: ~A)" (= 25 (attr-get vm :R0)) (comp program))
)