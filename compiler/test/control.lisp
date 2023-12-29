(require "compiler/compiler.lisp")
(require "vm/vm.lisp")

(let ((vm '()) (program '(progn (* 15 10) (/ 100 10))))
  (vm-init vm)                                  ; Initialisation de la VM
  (vm-load vm (comp program))              ; Compilation et chargement de la séquence dans la VM
  (vm-execute vm)                               ; Exécution du programme)
  (format t "SEQUENCE ~A: ~A~%" program (= 10 (attr-get vm :R0))))


(let ((vm '()) (program '(if (>= 15 10) (* 10 2) (+ 1 1))))
  (vm-init vm)                                  ; Initialisation de la VM
  (vm-load vm (comp program))                   ; Compilation et chargement du programme dans la VM
  (vm-execute vm)                               ; Exécution du programme
  (format t "IF ~A: ~A~%" program (= (attr-get vm :R0) 20)))

(let ((vm '()) (program '(progn (setf count 0) (while (< count 5) (setf count (+ count 1))) (+ count 0))))
  (vm-init vm)
  (vm-load vm (comp program))
  (vm-execute vm)
  (format t "WHILE ~A: ~A~%" program (= (attr-get vm :R0) 5)))

(let ((vm '()) (program '(progn (setf count 0) (for (setf i 0) (< i 5) (setf i (+ i 1)) (setf count (+ count 1))) (+ count 0))))
  (vm-init vm)
  (vm-load vm (comp program))
  (vm-execute vm)
  (format t "FOR ~A: ~A~%" program (= (attr-get vm :R0) 5))
)