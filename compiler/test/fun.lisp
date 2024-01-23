(require "compiler/compiler.lisp")
(require "vm/vm.lisp")

;; (let ((vm '()) (func '(defun add (hello) (+ 3 hello))) (call '(add 5)))
;;   (vm-init vm)

;;   (vm-load vm (comp func))
;;   (vm-load vm (comp call))
;;   (vm-execute vm)

;;   (format t "Test de fonction simple: ~A~%" (= 8 (attr-get vm :R0)))
;; )

;; (let ((vm '()) (func '(defun add (hello) (+ hello 3))) (call '(add (add (add 5)))))
;;   (vm-init vm)
;;   (vm-load vm (comp func))
;;   (vm-load vm (comp call))
;;   (vm-execute vm)

;;   (format t "Test de fonction simple: ~A~%" (= 14 (attr-get vm :R0)))
;; )

(let (
  (vm '())
  (func '(defun sum (n) (
    if (= n 0) 0 (+ n (sum (- n 1)))
  )))
  (call '(sum 10))
)
  (vm-init vm)
  (vm-load vm (comp func))
  (vm-load vm (comp call))
  (vm-execute vm)

  (format t "Somme n: ~A~%" (attr-get vm :R0))
)

(let (
  (vm '())
  (func '(defun fibonacci (n) (
    if (= n 0) 0
      (if (<= n 2) 1
        (+ (fibonacci (- n 2)) (fibonacci (- n 1)))
      )
  )))
  (call '(fibonacci 20))
)
  (vm-init vm)
  (vm-reset vm 1000000)
  (vm-load vm (comp func))
  (vm-load vm (comp call))
  (time (vm-execute vm))

  (format t "WARMUP FIBO(20): ~A~%" (attr-get vm :R0))
)

(let (
  (vm '())
  (func '(defun fact (n) (if (= n 0) 1 (* n (fact (- n 1))))))
  (call '(fact 150))
)
  (vm-init vm)
  (vm-reset vm 1000000)
  (vm-load vm (comp func))
  (vm-load vm (comp call))
  (time (vm-execute vm))

  (format t "Fact(250): ~A~%" (attr-get vm :R0))
)

(let (
  (vm '())
  (func '(defun fibonacci (n) 
           (cond 
             ((= n 0) 0)
             ((<= n 2) 1)
             (t (+ (fibonacci (- n 2)) (fibonacci (- n 1))))
           )))
  (call '(fibonacci 25))
)
  (vm-init vm)
  (vm-reset vm 1000000)
  (vm-load vm (comp func))
  (vm-load vm (comp call))
  (time (vm-execute vm))

  (format t "FIBO(25): ~A~%" (attr-get vm :R0))
)

(let (
  (vm '())
  (call '(if (numberp 1) 3 0))
)
  (vm-init vm)
  (vm-load vm (comp call))
  (vm-execute vm)

  (format t "Fonction CLISP: ~A~%" (= (attr-get vm :R0) 3))
)