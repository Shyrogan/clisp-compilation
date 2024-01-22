(require "compiler/compiler.lisp")
(require "vm/vm.lisp")

(let ((vm '()) (program '(and (>= 15 10) (> 5 2) )))
  (vm-init vm)                                  ; Initialisation de la VM
  (vm-load vm (comp program))                   ; Compilation et chargement du programme dans la VM
  (vm-execute vm)                               ; Exécution du programme
  (format t "AND ~A: ~A~%" program (= (attr-get vm :R0) 1)))

(let ((vm '()) (program '(and (>= 15 10) (not (> 5 2)) )))
  (vm-init vm)                                  ; Initialisation de la VM
  (vm-load vm (comp program))                   ; Compilation et chargement du programme dans la VM
  (vm-execute vm)                               ; Exécution du programme
  (format t "NOT ~A: ~A~%" program (= (attr-get vm :R0) 0)))


(let ((vm '()) (program '(or (>= 15 10) (> 0 2) )))
  (vm-init vm)                                  ; Initialisation de la VM
  (vm-load vm (comp program))                   ; Compilation et chargement du programme dans la VM
  (vm-execute vm)                               ; Exécution du programme
  (format t "OR ~A: ~A~%" program (= (attr-get vm :R0) 1)))

(let ((vm '()) (program '(or (>= 0 10) (> 0 2) )))
  (vm-init vm)                                  ; Initialisation de la VM
  (vm-load vm (comp program))                   ; Compilation et chargement du programme dans la VM
  (vm-execute vm)                               ; Exécution du programme
  (format t "OR ~A: ~A~%" program (= (attr-get vm :R0) 0)))
