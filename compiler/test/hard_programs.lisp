(require "compiler/compiler.lisp")
(require "vm/vm.lisp")

(let (
  (vm '())
  (func '(defun process-list (my-list)
    (let ((new-list (cons 100 my-list)))  ; Ajoute 100 au début de la liste
      (setf new-list (reverse new-list))   ; Inverse la liste
      new-list                             ; Retourne la liste transformée
  )))
  (call '(process-list (list 1 2 3 4)))    ; Appel avec la liste (1 2 3 4)
)
  (vm-init vm)
  (vm-load vm (comp func))
  (vm-load vm (comp call))
  (vm-execute vm)

  (format t "Test complexe de la liste: ~A~%" (equal '(4 3 2 1 100) (attr-get vm :R0)))
)