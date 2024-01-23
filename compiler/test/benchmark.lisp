(require "compiler/compiler.lisp")
(require "vm/vm.lisp")

(defun temps-moyen-en-ms (total-time)
  (let ((ticks-par-ms (/ internal-time-units-per-second 1000)))
    (/ total-time ticks-par-ms)))

(defun benchmark-fibonacci ()
  (loop for n from 0 to 25 do
    (format t "Benchmark pour FIBO(~A):~%" n)
    (let ((temps-total 0))
      (loop for i from 1 to 5 do
        (let ((vm '()))
          (vm-init vm)
          (vm-reset vm 1000000)
          (vm-load vm (comp '(defun fibonacci (n) 
                               (cond 
                                 ((= n 0) 0)
                                 ((<= n 2) 1)
                                 (t (+ (fibonacci (- n 2)) (fibonacci (- n 1))))))))
          (vm-load vm (comp `(fibonacci ,n)))
          (let ((debut-temps (get-internal-real-time)))
            (vm-execute vm)
            (let ((fin-temps (get-internal-real-time)))
              (setq temps-total (+ temps-total (- fin-temps debut-temps)))))))
      (format t "Temps moyen pour FIBO(~A): ~A ms~%" n (temps-moyen-en-ms (/ temps-total 5))))))

(benchmark-fibonacci)
