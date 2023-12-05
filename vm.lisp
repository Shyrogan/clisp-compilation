(require "vm-utils.lisp")

(defun vm-reset ( vm  &optional(size 1000))
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
         
    )
)

(defun vm-init ( vm &optional(size 1000))
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
            (loop while (>= initial-pc (vm-get vm :LAST_CODE))  ;; Continue tant que PC est dans les limites de la mémoire.
                do (let ((instr (aref mem initial-pc)))  ;; Récupère l'instruction comme une liste!  (LOAD 'R1 10).
                    (cond
                        ;; Exemple : Si l'instruction est un certain type, effectuez une action.
                        ;; Là on compare les instructions
                        ((equal (first instr) 'LOAD) (handle-load vm instr))
                        ((equal (first instr) 'STORE) (handle-store  vm instr))
                        ((equal (first instr) 'MOVE) (handle-move vm))
                        ((equal (first instr) 'ADD) (handle-add vm))
                        ((equal (first instr) 'SUB) (handle-sub vm))
                        ((equal (first instr) 'MUL) (handle-mul vm))
                        ((equal (first instr) 'DIV) (handle-div vm))
                        ((equal (first instr) 'INCR) (handle-incr vm))
                        ((equal (first instr) 'PUSH) (handle-push vm))
                        ((equal (first instr) 'POP) (handle-pop vm))
                        ((equal (first instr) 'LABEL) (handle-label vm))
                        ((equal (first instr) 'JMP) (handle-jmp vm))
                        ((equal (first instr) 'JSR) (handle-jsr vm))
                        ((equal (first instr) 'RTN) (handle-rtn vm))
                        ((equal (first instr) 'CMP) (handle-cmp vm))
                        ((equal (first instr) 'JGT) (handle-jgt vm))
                        ((equal (first instr) 'JGE) (handle-jge vm))
                        ((equal (first instr) 'JLT) (handle-jlt vm))
                        ((equal (first instr) 'JLE) (handle-jle vm))
                        ((equal (first instr) 'JEQ) (handle-jeq vm))
                        ((equal (first instr) 'JNE) (handle-jne vm))
                        ((equal (first instr) 'TEST) (handle-test vm))
                        ((equal (first instr) 'JTRUE) (handle-jtrue vm))
                        ((equal (first instr) 'JNIL) (handle-jnil vm)) 
                        ((equal (first instr) 'NOP) (handle-nop vm))
                        ((equal (first instr) 'HALT) (handle-halt vm))

                        (t (format t "Instruction inconnue: ~A~%" instr))) ;;pour les cas d'erreurs
                    ;; Incrémente PC pour passer à l'instruction suivante.
                   (setq initial-pc (- initial-pc 1))
                )
            )

            (vm-set vm :PC (vm-get vm :PC)) )  ;; Réinitialise le PC à la position initiale.
        )

(defun handle-load (vm instr)
  ;; Assume instr is like (LOAD 'R1 10)
  (let ((reg (second instr))  ;; 'R1
        (val (third instr)))  ;; 10
    ;; Execute the load - store `val` in the register `reg`
    (vm-set vm reg val)
    (format t "Loaded ~A into ~A~%" val reg))
)

(defun handle-store (vm instr)
    ;; Assume instr is like (STORE 'R1 100)
    ;; Where 'R1 is the register and 100 is the memory address
    (let ((reg (second instr))   ;; Extracts the register (e.g., 'R1)
        (adr (third instr)))   ;; Extracts the memory address (e.g., 100)
    ;; Retrieve the value stored in the register
        (let ((val (vm-get vm reg)))
            ;; Store the value from the register to the specified memory address
            (mem-set vm adr val)
            (format t "Stored ~A from ~A to memory address ~A~%" val reg adr))
    )
)
