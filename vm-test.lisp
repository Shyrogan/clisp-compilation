(require "vm.lisp")

;; Chargement de constante
(let  ((vm '())) 
      (vm-init vm)
      (vm-load vm '(
        (LOAD (:CONST 10) :R1)
      ))
      (vm-execute vm)

      ;; Vérifier les résultats
      (print `(test Load constante, (= (vm-get vm :R1) 10)))
)

;; Chargement de mémoire direct
(let  ((vm '())) 
      (vm-init vm)
      (vm-load vm '(
        (LOAD 10 :R1)
      ))
      (mem-set vm 10 100)
      (vm-execute vm)

      ;; Vérifier les résultats
      (print `(test Load mémoire, (= (vm-get vm :R1) 100)))
)

;; Chargement de mémoire indirect
(let  ((vm '())) 
      (vm-init vm)
      (vm-load vm '(
        (LOAD :R2 :R1)
      ))
      (mem-set vm 10 100)
      (vm-set vm :R2 10)
      (vm-execute vm)

      ;; Vérifier les résultats
      (print `(test Load mémoire indirect, (= (vm-get vm :R1) 100)))
)

;; Chargement de mémoire indirect + offset
(let  ((vm '())) 
      (vm-init vm)
      (vm-load vm '(
        (LOAD (+ :R2 4) :R1)
      ))
      (mem-set vm 14 100)
      (vm-set vm :R2 10)
      (vm-execute vm)

      ;; Vérifier les résultats
      (print `(test Load mémoire indirect avec offset : ,(= (vm-get vm :R1) 100)))
)

;; Stockage de constante
(let ((vm '())) 
  (vm-init vm)
  (vm-load vm '(
    (STORE (:CONST 10) :R1)
  ))
  (vm-set vm :R1 10)
  (vm-execute vm)
  (print `(test Store constante : ,(= (mem-get vm 10) 10)))
)

;; Stockage de mémoire direct
(let ((vm '())) 
  (vm-init vm)
  (vm-load vm '((STORE :R1 10)))
  (vm-set vm :R1 100)
  (vm-execute vm)
  (print `(test Store mémoire : ,(= (mem-get vm 10) 100)))
)

;; Stockage de mémoire indirect
(let ((vm '())) 
  (vm-init vm)
  (vm-load vm '((STORE :R1 :R2)))
  (vm-set vm :R1 100)
  (vm-set vm :R2 10)
  (vm-execute vm)
  (print `(test Store mémoire indirect : ,(= (mem-get vm 10) 100)))
)

;; ;; Stockage de mémoire indirect + offset
(let ((vm '())) 
  (vm-init vm)
  (vm-load vm '((STORE :R1 (+ :R2 4))))
  (vm-set vm :R1 100)
  (vm-set vm :R2 10)
  (vm-execute vm)
  (print `(test Store mémoire indirect avec offset : ,(= (mem-get vm 14) 100)))
)

;; Test de transfert de constante dans un registre
(let ((vm '()))
  (vm-init vm)
  (vm-load vm '((MOVE (:CONST 50) :R1)))
  (vm-execute vm)
  (print `(test MOVE constante -> registre, (= (vm-get vm :R1) 50)))
)

;; Test de transfert de valeur entre registres
(let ((vm '()))
  (vm-init vm)
  (vm-set vm :R0 100)  ;; Valeur initiale du registre R0
  (vm-load vm '((MOVE :R0 :R1)))
  (vm-execute vm)
  (print `(test MOVE registre -> registre, (= (vm-get vm :R1) 100)))
)

;; Test pour ADD
(let ((vm '()))
  (vm-init vm)
  (vm-load vm '(
    (LOAD (:CONST 5) :R1)
    (LOAD (:CONST 10) :R2)
    (ADD :R1 :R2)         ; Mode direct
    (ADD (:CONST 20) :R1) ; Mode indirect
  ))
  (vm-execute vm)

  ;; Vérification des résultats
  (print `(test ADD direct : ,(= (vm-get vm :R2) 15)))
  (print `(test ADD indirect : ,(= (vm-get vm :R1) 25)))
)

;; Test pour SUB
(let ((vm '()))
  (vm-init vm)
  (vm-load vm '(
    (LOAD (:CONST 10) :R1)
    (LOAD (:CONST 20) :R2)
    (SUB :R1 :R2)         ; Mode direct
    (SUB (:CONST 5) :R1) ; Mode indirect
  ))
  (vm-execute vm)

  ;; Vérification des résultats
  (print `(test SUB direct : ,(= (vm-get vm :R2) 10)))
  (print `(test SUB indirect : ,(= (vm-get vm :R1) 5)))
)

;; Test pour MUL
(let ((vm '()))
  (vm-init vm)
  (vm-load vm '(
    (LOAD (:CONST 4) :R1)
    (LOAD (:CONST 3) :R2)
    (MUL :R1 :R2)         ; Mode direct
    (MUL (:CONST 5) :R1)  ; Mode indirect
  ))
  (vm-execute vm)

  ;; Vérification des résultats
  (print `(test MUL direct : ,(= (vm-get vm :R2) 12)))
  (print `(test MUL indirect : ,(= (vm-get vm :R1) 20)))
)

;; Test pour DIV
(let ((vm '()))
  (vm-init vm)
  (vm-load vm '(
    (LOAD (:CONST 2) :R1)
    (LOAD (:CONST 6) :R2)
    (DIV :R1 :R2)         ; Mode direct
    (DIV (:CONST 3) :R1)  ; Mode indirect
  ))
  (vm-execute vm)

  ;; Vérification des résultats
  (print `(test DIV direct : ,(= (vm-get vm :R2) 3)))
  (print `(test DIV indirect : ,(= (vm-get vm :R1) 2/3)))
)

;; Test du stack direct
(let ((vm '()))
  (vm-init vm)
  (vm-load vm '(
    (PUSH (:CONST 5))  ; Push constant 5 onto the stack
    (POP :R1)  ; Pop the stack into register R1
  ))
  (vm-execute vm)
  (print `(test PUSH/POP : ,(= (vm-get vm :R1) 5)))
)

;; Test du stack indirect
(let ((vm '()))
  (vm-init vm)
  (vm-load vm '(
    (PUSH :R0)  ; Push the value in register R0 onto the stack
    (POP :R1)   ; Pop the stack into register R1
  ))
  (vm-set vm :R0 10)  ; Set R0 value to 10
  (vm-execute vm)
  (print `(test PUSH/POP : ,(= (vm-get vm :R1) 10)))
)

;; Factoriel
(let ((vm '()))
  ; Initialisation de la machine virtuelle et des instructions
  (vm-init vm)
  (vm-load vm '(
    (LOAD (:CONST 10) :R0)     ; Charge le nombre qu'on veut dans R1 pour le factoriel
    (LOAD (:CONST 1) :R1)     ; Charge 1 dans R2 pour la multiplication
    (LOAD (:CONST 1) :R2)     ; Charge 1 dans R3 pour la boucle

    (INCR :R0) ;; En gros si on veut fact(5), il faut itérer de 1 à 6 (5 fois)
    ; Début de la boucle
    (LABEL start)
    (CMP :R2 :R0)
    (JEQ exit)      ; Si R3 = R1, sortir de la boucle
    (MUL :R2 :R1)        ; R1 = R1 * R2 (calcul du factoriel)
    (INCR :R2)               ; Incrémente R2 (incrément pour la boucle)
    (JMP start)             ; Retour au début de la boucle

    ; Sortie
    (LABEL exit)
    (HALT)
  ))
  (vm-execute vm)
  (print (vm-get vm :R1))
)