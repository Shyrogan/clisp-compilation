(require "vm.lisp")


  ;; Créer une instance de la VM et l'initialiser
(let  ((vm '()))
      (vm-init vm)
      (print (= (get vm :FGT ) 0)) ;; pour voir si les valeur on était initialiser
      
      (setf (get vm :PC) 5) ;; on modifie pc
      (vm-reset vm)
      (print  (= (get vm :PC ) 999) ) ;; pour voir que vm-reset a fait son travail
  
  ;; Charger un programme de test dans la VM
      (vm-load vm '(instr1 instr2 instr3 ))

  ;; Exécuter le programme (supposant que vous avez une fonction vm-execute)
  ;;(vm-execute my-vm)

  ;; Vérifier les résultats
  ;;(assert (= (vm-get my-vm :R0) expected-value))
  ;; Ajouter plus de vérifications au besoin
  )
