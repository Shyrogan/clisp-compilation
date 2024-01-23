(require "compiler/compiler.lisp")
(require "vm/vm.lisp")

(let (
  (vm '())
  (func '(defun sum (n) (
    if (= n 0) 0 n)
  ))
  (call '(sum 10))
)
  (vm-init vm)
  (vm-load vm (comp func))
  (vm-load vm (comp call))
  (format t "~A~%" (attr-get vm :MEM))
  (set-debug vm)
  
  (vm-execute vm)

  (format t "Fonction avec condition: ~A~%" (attr-get vm :R0))
)