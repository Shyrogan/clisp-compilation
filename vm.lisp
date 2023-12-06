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
    (let ((mem (vm-get vm :MEM)))         ;; Récupère la mémoire de la VM.) 
        (loop while (>= (vm-get vm :PC) (vm-get vm :LAST_CODE))  ;; Continue tant que PC est dans les limites de la mémoire.
            do (let ((instr (aref mem (vm-get vm :PC))))  ;; Récupère l'instruction comme une liste!  (LOAD 'R1 10).
                (cond
                        ;; Exemple : Si l'instruction est un certain type, effectuez une action.
                        ;; Là on compare les instructions
                        ((equal (first instr) 'LOAD) (handle-load vm instr))
                        ((equal (first instr) 'STORE) (handle-store  vm instr))
                        ((equal (first instr) 'MOVE) (handle-move vm instr))
                        ((equal (first instr) 'ADD) (handle-add vm instr))
                        ((equal (first instr) 'SUB) (handle-sub vm instr))
                        ((equal (first instr) 'MUL) (handle-mul vm instr))
                        ((equal (first instr) 'DIV) (handle-div vm instr))
                        ((equal (first instr) 'INCR) (handle-incr vm instr))
                        ((equal (first instr) 'PUSH) (handle-push vm instr))
                        ((equal (first instr) 'POP) (handle-pop vm instr))
                        ((equal (first instr) 'LABEL) (handle-label vm instr))
                        ((equal (first instr) 'JMP) (handle-jmp vm instr))
                        ((equal (first instr) 'JSR) (handle-jsr vm instr))
                        ((equal (first instr) 'RTN) (handle-rtn vm instr))
                        ((equal (first instr) 'CMP) (handle-cmp vm instr))
                        ((equal (first instr) 'JGT) (handle-jgt vm instr))
                        ((equal (first instr) 'JGE) (handle-jge vm instr))
                        ((equal (first instr) 'JLT) (handle-jlt vm instr))
                        ((equal (first instr) 'JLE) (handle-jle vm instr))
                        ((equal (first instr) 'JEQ) (handle-jeq vm instr))
                        ((equal (first instr) 'JNE) (handle-jne vm instr))
                        ((equal (first instr) 'TEST) (handle-test vm instr))
                        ((equal (first instr) 'JTRUE) (handle-jtrue vm instr))
                        ((equal (first instr) 'JNIL) (handle-jnil vm instr)) 
                        ((equal (first instr) 'NOP) (handle-nop vm instr))
                        ((equal (first instr) 'HALT) (handle-halt vm instr))
                        (t (format t "Instruction inconnue: ~A~%" instr))) ;;pour les cas d'erreurs
                    ;; Incrémente PC pour passer à l'instruction suivante.
                   (vm-set vm :PC (- (vm-get vm :PC) 1))
                )
            ) 
    )
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

(defun handle-move (vm instr) ;;(MOVE 'R1 'R2) copies the value from R1 to R2
  (let ((src-reg (second instr))
        (dest-reg (third instr)))
    (vm-set vm dest-reg (vm-get vm src-reg)))
)

(defun handle-add (vm instr) ;;(ADD 'R1 'R2) adds the values R1 and R2, storing the result in R2
  (let ((reg1 (second instr))
        (reg2 (third instr)))
    (vm-set vm reg2 (+ (vm-get vm reg1) (vm-get vm reg2))))
)

(defun handle-sub (vm instr)
  (let ((reg1 (second instr))
        (reg2 (third instr)))
    (vm-set vm reg2 (- (vm-get vm reg1) (vm-get vm reg2))))
)

(defun handle-mul (vm instr)
  (let ((reg1 (second instr))
        (reg2 (third instr)))
    (vm-set vm reg2 (* (vm-get vm reg1) (vm-get vm reg2))))
)

(defun handle-div (vm instr)
    (let ((reg1 (second instr))
        (reg2 (third instr)))
    (vm-set vm reg2 (/ (vm-get vm reg1) (vm-get vm reg2))))
)

(defun handle-incr (vm instr)
  (let ((reg (second instr)))
    (vm-set vm reg (+ (vm-get vm reg) 1)))
)

#| pas fini |#
(defun handle-push (vm instr)
  (let ((reg (second instr)))
    (let ((val (vm-get vm reg)))
      (let ((sp (vm-get vm :SP)))
        (mem-set vm sp val)
        (vm-set vm :SP (+ sp 1)))))
)

#| pas fini |#
(defun handle-pop (vm instr)
  (let ((reg (second instr)))
    (let ((sp (vm-get vm :SP)))
      (vm-set vm reg (get-mem vm (- sp 1)))
      (vm-set vm :SP (- sp 1))))
)

#| pas fini |#
(defun handle-label (vm instr)
  ;; Label handling code
)

#| pas fini (ça boucle) |#
(defun handle-jmp (vm instr)
    (let ((target (second instr)))
    (vm-set vm :PC (+ target 1)) ;; +1 car on fait -1 à la fin de l'execution de chaque instructino 
    )
)

#| pas fini |#
(defun handle-jsr (vm instr)
  ;; JSR handling code
)

#| pas fini |#
(defun handle-rtn (vm instr)
  ;; RTN handling code
)

#| pas fini |#
(defun handle-cmp (vm instr)
  (let ((reg1 (second instr))
        (reg2 (third instr)))
    (let ((val1 (vm-get vm reg1))
          (val2 (vm-get vm reg2)))
      (vm-set vm :FEQ (= val1 val2))
      (vm-set vm :FLT (< val1 val2))
      (vm-set vm :FGT (> val1 val2))))
)

#| pas fini |#
(defun handle-jgt (vm instr)
  (when (> (vm-get vm :FGT) 0)
    (vm-set vm :PC (second instr)))
)

#| pas fini |#
(defun handle-jge (vm instr)
  (when (or (> (vm-get vm :FGT) 0) (= (vm-get vm :FEQ) 1))
    (vm-set vm :PC (second instr)))
)

#| pas fini |#
(defun handle-jlt (vm instr)
  (when (< (vm-get vm :FLT) 0)
    (vm-set vm :PC (second instr)))
)

#| pas fini |#
(defun handle-jle (vm instr)
  (when (or (< (vm-get vm :FLT) 0) (= (vm-get vm :FEQ) 1))
    (vm-set vm :PC (second instr)))
)

#| pas fini |#
(defun handle-jeq (vm instr)
  (when (= (vm-get vm :FEQ) 1)
    (vm-set vm :PC (second instr)))
)

#| pas fini |#
(defun handle-jne (vm instr)
  (when (/= (vm-get vm :FEQ) 1)
    (vm-set vm :PC (second instr)))
)

#| pas fini |#
(defun handle-test (vm instr)
  ;; Test handling code
)

#| pas fini |#
(defun handle-jtrue (vm instr)
  (when (> (vm-get vm (second instr)) 0)
    (vm-set vm :PC (third instr))))

#| pas fini |#
(defun handle-jnil (vm instr)
  (when (= (vm-get vm (second instr)) 0)
    (vm-set vm :PC (third instr))))

#| pas fini |#
(defun handle-nop (vm instr)
  ;; NOP handling code
)

#| pas fini |#
(defun handle-halt (vm instr)
  ;; HALT handling code
)
