(require "vm.lisp")
 
  ;; Créer une instance de la VM et l'initialiser
(let  ((vm '())) 
      (vm-init vm)
      
      (print '( test vm-init :) )
      (print (= (get vm :FGT ) 0)) ;; test init
      
      (setf (get vm :PC) 5) ;; on modifie pc
      
      (print '( test vm-resert :) )
      (print  (= (get vm :PC ) 999) ) ;; test execute
  
 
      (vm-load vm '((LOAD :R1 10 ) (STORE :R1 10 ) ))
      (vm-execute  vm)

      ;; Vérifier les résultats
      (print '( tests for load and execute :) )
      (print (= (get vm :R1 ) 10)) 
      (print (= (mem-get vm 10) 10 ))  
  )

(let  ((vm '())) 
      (vm-init vm)
      (vm-load vm '((LOAD :R1 10 ) (MOVE :R1 :R2) ))
      (vm-execute  vm)

      ;; Vérifier les résultats
      (print '( test Move :) )
      (print (= (vm-get vm :R2) 10))
  )

(let  ((vm '())) 
      (vm-init vm)
      (vm-load vm '((LOAD :R1 10 ) (LOAD :R2 5) (ADD :R1 :R2) ))
      (vm-execute  vm)

      ;; Vérifier les résultats
      (print '( test ADD :) )
      (print (= (vm-get vm :R2) 15))
  )

(let  ((vm '())) 
      (vm-init vm)
      (vm-load vm '(
        (LOAD :R1 5)
        (LOAD :R2 10)
        (SUB :R1 :R2)
      ))
      (vm-execute  vm)

      ;; Vérifier les résultats
      (print '( test SUB :) )
      (print (= (vm-get vm :R2) 5))
  )

(let  ((vm '())) 
      (vm-init vm)
      (vm-load vm '(
        (LOAD :R1 10)
        (LOAD :R2 5)
        (MUL :R1 :R2)
      ))
      (vm-execute  vm)

      ;; Vérifier les résultats
      (print '( test MUL :) )
      (print (= (vm-get vm :R2) 50))
  )

(let  ((vm '())) 
      (vm-init vm)
      (vm-load vm '(
        (LOAD :R1 10 )
        (LOAD :R2 5)
        (DIV :R1 :R2)
      ))
      (vm-execute  vm)

      ;; Vérifier les résultats
      (print '( test DIV :) )
      (print (= (vm-get vm :R2) 2))
  )

(let  ((vm '())) 
      (vm-init vm)
      (vm-load vm '((LOAD :R1 10 ) (INCR :R1)))
      (vm-execute  vm)

      ;; Vérifier les résultats
      (print '( test INCR :) )
      (print (= (vm-get vm :R1) 11))
  )

(let  ((vm '())) 
      (vm-init vm)
      (vm-load vm '(
        (LABEL label)
        (LOAD :R1 10)
        (INCR :R1)
        (INCR :R1)
        (LOAD :R2 5)
        (SUB :R2 :R1)
        (INCR :R1)
        (JMP label)
      ))
      (vm-execute  vm)

      ;; Vérifier les résultats
      (print '( test JMP :) )
      (print (vm-get vm :ETIQ))
  )
