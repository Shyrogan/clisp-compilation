(require "vm.lisp")


  ;; Créer une instance de la VM et l'initialiser
(let  ((vm '()))
      (vm-init vm)
      (print (= (get vm :FGT ) 0)) ;; pour voir si les valeur on était initialiser
      
      (setf (get vm :PC) 5) ;; on modifie pc
      (vm-reset vm)
      (print  (= (get vm :PC ) 999) ) ;; pour voir que vm-reset a fait son travail
  
      ;; Charger un programme de test dans la VM
      (vm-load vm '((LOAD :R1 10 ) (STORE :R1 10 ) ))
      ;; Exécuter le programme (supposant que vous avez une fonction vm-execute)
      (vm-execute  vm)

      ;; Vérifier les résultats
      (print (= (get vm :R1 ) 10)) 
      (print (= (get-mem vm 10) 10 )) 
      
      (print vm) ;; ca marche pas!!!!!!!!!!!!!!!!!!!!!!
      (afficher-registres vm) ;; ca marche pas!!!!!!!!!!!!!!!!!!!!!! jppp!

  )










