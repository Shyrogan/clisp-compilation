(require "vm.lisp")


 
  ;; Créer une instance de la VM et l'initialiser
(let  ((vm '()))
      
      (vm-init vm)
      
      (print '( test vm-init :) )
      (print (= (get vm :FGT ) 0)) ;; test init
      
      (setf (get vm :PC) 5) ;; on modifie pc
      (vm-reset vm)
      
      (print '( test vm-resert :) )
      (print  (= (get vm :PC ) 999) ) ;; test execute
  
 
      (vm-load vm '((LOAD :R1 10 ) (STORE :R1 10 ) ))
      (vm-execute  vm)

      ;; Vérifier les résultats
      (print '( tests for load and execute :) )
      (print (= (get vm :R1 ) 10)) 
      (print (= (get-mem vm 10) 10 )) 
      
      (print '( cant print the vm dont know why :) )
      (print vm) ;; ca marche pas!!!!!!!!!!!!!!!!!!!!!!
      (afficher-registres vm) ;; ca marche pas!!!!!!!!!!!!!!!!!!!!!! jppp!

  )












