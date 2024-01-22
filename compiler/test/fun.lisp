(require "compiler/compiler.lisp")
(require "vm/vm.lisp")

(let ((vm '()) (func '(defun add (hello) (+ 3 hello))) (call '(add 5)))
  (vm-init vm)

  (vm-load vm (comp func))
  (vm-load vm (comp call))
  (vm-execute vm)

  (format t "Test de fonction simple: ~A~%" (= 8 (attr-get vm :R0)))
)

(let ((vm '()) (func '(defun add (hello) (+ hello 3))) (call '(add (add (add 5)))))
  (vm-init vm)

  (vm-load vm (comp func))
  (vm-load vm (comp call))
  (vm-execute vm)

  (format t "Test de fonction simple: ~A~%" (= 14 (attr-get vm :R0)))
)

(let (
  (vm '())
  (func '(defun sum (n) (
    if (= n 0) 0 (+ n (sum(- n 1)))
  )))
  (call '(sum 10))
)
  (vm-init vm)

  (vm-load vm (comp func))
  (vm-load vm (comp call))
  (vm-execute vm)

  (format t "Somme n: ~A~%" (= 55 (attr-get vm :R0)))
)

(let (
  (vm '())
  (func '(defun fibonacci (n) (
    if (= n 0) 0
      (if (<= n 2) 1
        (+ (fibonacci (- n 2)) (fibonacci (- n 1)))
      )
  )))
  (call '(fibonacci 15))
)
  (vm-init vm)
  (vm-load vm (comp func))
  (vm-load vm (comp call))
  (vm-execute vm)

  (format t "Fibonacci: ~A~%" (= 610 (attr-get vm :R0)))
)

(let (
  (vm '())
  (call '(numberp 10))
)
  (vm-init vm)
  (vm-load vm (comp call))
  (vm-execute vm)

  (format t "Zaza: ~A~%" (attr-get vm :R0))
)