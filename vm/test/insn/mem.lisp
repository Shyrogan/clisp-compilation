(require "vm/vm.lisp")

;; Différents load
(let (vm '())
  (vm-init vm)
  (vm-load vm '(
    (LOAD R0 R1)
    (LOAD 2 R0)
    (LOAD (+ R1 1) R2)
  ))
  (attr-set vm :R0 1)
  (mem-set vm 1 10)
  (mem-set vm 2 15)
  (mem-set vm 11 20)
  (vm-execute vm)
  (format t "Test LOAD DIRECT: ~A~%" (= (attr-get vm :R0) 15))
  (format t "Test LOAD INDIRECT: ~A~%" (= (attr-get vm :R1) 10))
  (format t "Test LOAD INDIRECT+OFFSET: ~A~%" (= (attr-get vm :R2) 20)))

;; Différents store
(let (vm '())
  (vm-init vm)
  (vm-load vm '(
    (STORE (:CONST 2) R0)
    (STORE R1 2)
    (STORE (:CONST 10) (+ R0 2))
  ))
  (mem-set vm 1 2)
  (attr-set vm :R0 1)
  (attr-set vm :R1 4)
  (vm-execute vm)
  (format t "Test STORE DIRECT: ~A~%" (= (mem-get vm 1) 2))
  (format t "Test STORE INDIRECT: ~A~%" (= (mem-get vm 2) 4))
  (format t "Test STORE INDIRECT+OFFSET: ~A~%" (= (mem-get vm 3) 10)))