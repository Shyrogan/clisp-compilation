(require "compiler/compiler.lisp")
(require "vm/vm.lisp")

(let ((vm '()) (program '(progn (* 15 10) (/ 100 10))))
  (vm-init vm)                                  ; Initialisation de la VM
  (vm-load vm (comp program))              ; Compilation et chargement de la séquence dans la VM
  (vm-execute vm)                               ; Exécution du programme)
  (format t "Une succession d'expression arithmétique ~A: ~A~%" program (= 10 (attr-get vm :R0))))


(let ((vm '()) (fib '()))
  (vm-init vm)                                  ; Initialisation de la VM
  (vm-load vm (comp program))                   ; Compilation et chargement du programme dans la VM
  (vm-execute vm)                               ; Exécution du programme
  (format t "Résultat de l'évaluation de l'expression ~A: ~A~%" program (= (attr-get vm :R0) 20)))
