(require "vm-utils.lisp")

(defun vm-reset (&optional (vm 'machine-virtuel) (size 1000))
    ;; On veut une taille d'au moins 1000 su notre VM, histoire qu'elle soit fonctionnelle.
    (let ((taille (max size 1000)))
        ;; On stocke la taille
        (vm-set vm :SIZE taille)
        ;; On réserve la mémoire dans un tableau
        (vm-set vm :MEM (make-array taille))
        ;; On initialise le program counter
        (vm-set vm :PC (- taille 1))
        (vm-set vm :ETIQ (make-hash-table :size taille))
        (vm-set vm :UKNWN_ETIQ (make-hash-table :size taille))
        ;; (set-etiqNR vm 'nb 0)
         
    ))

(defun vm-init (&optional (vm 'machine-virtuel) (size 1000))
    ;; Est-ce que c'est vraiment utile?
    (vm-set vm :NAME vm)
    ;; Registres
    (vm-set vm :R0 0)
    (vm-set vm :R1 0)
    (vm-set vm :R2 0)
    (vm-set vm :R3 0)
    ;; Base pointer: pointe vers le début de la pile.
    (vm-set vm :BP 0)
    ;; Stack pointer: pointe généralement vers le sommet de la pile dans une architecture informatique basée sur 
    ;; la pile. Il est utilisé pour suivre la position actuelle de la pile en mémoire.
    (vm-set vm :SP 0)
    ;; Program counter: pointe vers l'adresse de l'instruction suivante à exécuter dans la séquence d'instructions 
    ;; du programme en cours.
    (vm-set vm :PC 0)
    ;; Line counter: 
    (vm-set vm :LC 0)
    ;; Frame pointer: pointe vers le début d'une frame (ou cadre) de pile pour une fonction donnée dans un programme.
    ;; LEC: sert à définir des blocs de pile pour la structurer et faciliter les accès. 
    (vm-set vm :FP 0)
    ;; Flag less than: drapeau plus petit
    (vm-set vm :FLT 0)
    ;; Flag less than: drapeau égal
    (vm-set vm :FEQ 0) ;; Equal FEQ
    ;; Flag less than: drapeau plus grand
    (vm-set vm :FGT 0)
    (vm-reset vm size)
    )

    (defun vm-load (vm program)
  ;; Charge un programme dans la mémoire de la machine virtuelle.
        (let ((mem (vm-get vm :MEM))         ;; Récupère la mémoire de la VM.
        (initial-pc (vm-get vm :PC)))  ;; Sauvegarde la valeur initiale du PC.
            (loop for instr in program do
                (setf (aref mem initial-pc) instr) ;; Place chaque instruction dans la mémoire.
                (setq initial-pc (- initial-pc 1))  ;; Decrémente la position de la mémoire. 999->998->997
            )
            (vm-set vm :LAST_CODE (+ initial-pc 1)) ;; on a enregistré l'adress de la dernière instruction! et le début 
            (vm-set vm :PC (vm-get vm :PC))   ;; Réinitialise le PC à la position initiale.
        )   
    )

     (defun vm-execute (vm)
  ;; Execution Engine: Charge d'éxecuter les instructions une après l'autre 
        (let ((mem (vm-get vm :MEM))         ;; Récupère la mémoire de la VM.
        (initial-pc (vm-get vm :PC)))  ;; Sauvegarde la valeur initiale du PC.
            (loop while (>= pc (vm-get vm :LAST_CODE))  ;; Continue tant que PC est dans les limites de la mémoire.
                do (let ((instr (aref mem pc)))  ;; Récupère l'instruction actuelle.
                    (cond
                        ;; Exemple : Si l'instruction est un certain type, effectuez une action.
                        ;; Là on compare les instructions
                        ((equal instr 'LOAD) (handle-instr1 vm))
                        ((equal instr 'STORE) (handle-instr2 vm))
                        ((equal instr 'MOVE) (handle-instr1 vm))
                        ((equal instr 'ADD) (handle-instr2 vm))
                        ((equal instr 'SUB) (handle-instr1 vm))
                        ((equal instr 'MUL) (handle-instr2 vm))
                        ((equal instr 'DIV) (handle-instr1 vm))
                        ((equal instr 'INCR) (handle-instr2 vm))
                        ((equal instr 'PUSH) (handle-instr1 vm))
                        ((equal instr 'POP) (handle-instr2 vm))
                        ((equal instr 'LABEL) (handle-instr1 vm))
                        ((equal instr 'JMP) (handle-instr2 vm))
                        ((equal instr 'JSR) (handle-instr1 vm))
                        ((equal instr 'RTN) (handle-instr2 vm))
                        ((equal instr 'CMP) (handle-instr1 vm))
                        ((equal instr 'JGT) (handle-instr2 vm))
                        ((equal instr 'JGE) (handle-instr1 vm))
                        ((equal instr 'JLT) (handle-instr2 vm))
                        ((equal instr 'JLE) (handle-instr1 vm))
                        ((equal instr 'JEQ) (handle-instr2 vm))
                        ((equal instr 'JNE) (handle-instr1 vm))
                        ((equal instr 'TEST) (handle-instr2 vm))
                        ((equal instr 'JTRUE) (handle-instr1 vm))
                        ((equal instr 'JNIL) (handle-instr2 vm))
                        ((equal instr 'NOP) (handle-instr1 vm))
                        ((equal instr 'HALT) (handle-instr2 vm))
                        (t (format t "Instruction inconnue: ~A~%" instr))) ;;pour les cas d'erreurs
                    ;; Incrémente PC pour passer à l'instruction suivante.
                   (setq initial-pc (- initial-pc 1))
                )
            )



            (vm-set vm :PC (vm-get vm :PC)) )  ;; Réinitialise le PC à la position initiale.
        
        
        
        )   
    
