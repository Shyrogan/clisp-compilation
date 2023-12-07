(require "vm-utils.lisp")

(defun vm-reset (vm  &optional(size 1000))
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

(defun vm-init (vm &optional(size 1000))
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
    (let ((initial-pc (pc-get vm)))  ;; Sauvegarde la valeur initiale du PC.
        (loop for instr in program do
            (cond 
              ((equal (first instr) 'LABEL) (progn (let ((label (string (second instr))))
                  (setf (gethash label (vm-get vm :ETIQ)) initial-pc)
                  (format t "Label ~A added with PC value ~A~%" label initial-pc))))
              (t (progn 
                (mem-set vm initial-pc instr) ;; Place chaque instruction dans la mémoire.
                (setq initial-pc (- initial-pc 1))  ;; Decrémente la position de la mémoire. 999->998->997))
              ))
            )
        )
        (setq initial-pc (pc-get vm))
        (loop for instr in program do
          (if (and (listp instr) (eq (first instr) 'JMP))
              (let* ((label (string (second instr))) (address (gethash label (vm-get vm :ETIQ))))
                (mem-set vm (+ initial-pc 1) (list 'JMP address))
                (format t "JUMP towards ~A will jump to ~A~%" label address)))
          (if (and (listp instr) (eq (first instr) 'JSR))
              (let* ((label (string (second instr))) (address (gethash label (vm-get vm :ETIQ))))
                (mem-set vm (+ initial-pc 1) (list 'JSR address))
                (format t "JSR towards ~A will jump to ~A~%" label address)))
          (if (and (listp instr) (eq (first instr) 'JLT))
              (let* ((label (string (second instr))) (address (gethash label (vm-get vm :ETIQ))))
                (mem-set vm (+ initial-pc 1) (list 'JLT address))
                (format t "JLT towards ~A will jump to ~A~%" label address)))
          (if (and (listp instr) (eq (first instr) 'JLE))
              (let* ((label (string (second instr))) (address (gethash label (vm-get vm :ETIQ))))
                (mem-set vm (+ initial-pc 1) (list 'JLE address))
                (format t "JLE towards ~A will jump to ~A~%" label address)))
          (if (and (listp instr) (eq (first instr) 'JGT))
              (let* ((label (string (second instr))) (address (gethash label (vm-get vm :ETIQ))))
                (mem-set vm (+ initial-pc 1) (list 'JGT address))
                (format t "JGT towards ~A will jump to ~A~%" label address)))
          (if (and (listp instr) (eq (first instr) 'JGE))
              (let* ((label (string (second instr))) (address (gethash label (vm-get vm :ETIQ))))
                (mem-set vm (+ initial-pc 1) (list 'JGE address))
                (format t "JGE towards ~A will jump to ~A~%" label address)))
          (if (and (listp instr) (eq (first instr) 'JEQ))
              (let* ((label (string (second instr))) (address (gethash label (vm-get vm :ETIQ))))
                (mem-set vm (+ initial-pc 1) (list 'JEQ address))
                (format t "JEQ towards ~A will jump to ~A~%" label address)))
          (if (and (listp instr) (eq (first instr) 'JNE))
              (let* ((label (string (second instr))) (address (gethash label (vm-get vm :ETIQ))))
                (mem-set vm (+ initial-pc 1) (list 'JNE address))
                (format t "JNE towards ~A will jump to ~A~%" label address)))
          (setq initial-pc (- initial-pc 1))
        )
        (vm-set vm :LAST_CODE (+ initial-pc 1)) ;; on a enregistré l'adress de la dernière instruction! et le début 
        (pc-set vm (pc-get vm))   ;; Réinitialise le PC à la position initiale.
    )
)

(defun vm-execute (vm)
    (loop while (>= (pc-get vm) (vm-get vm :LAST_CODE))
        do (let ((instr (mem-get vm (pc-get vm))))
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
                        ((equal (first instr) 'DECR) (handle-decr vm instr))
                        ((equal (first instr) 'PUSH) (handle-push vm instr))
                        ((equal (first instr) 'POP) (handle-pop vm instr))
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

(defun handle-load (vm instr)
  (let ((src (second instr))  ;; Source
        (dest (third instr))) ;; Destination
    (cond
      ;; Si la source est une constante (:CONST constante)
      ((and (listp src) (eq (first src) :CONST))
       (vm-set vm dest (second src)))

      ;; Si la source est une adresse mémoire (nombre donc adressage direct)
      ((numberp src)
       (vm-set vm dest (mem-get vm src)))
      
      ;; Si la source est un registre (indirect)
      ((symbolp src)
       (let ((mem-addr (vm-get vm src)))
         (vm-set vm dest (mem-get vm mem-addr))))
      
      ;; Si la source est un registre avec un offset positif
      ((and (listp src) (eq (first src) '+))
       (let* ((reg (second src))
              (offset (third src))
              (reg-val (vm-get vm reg)))
         (let ((address (+ reg-val offset)))
           (vm-set vm dest (mem-get vm address)))))
      
      ((and (listp src) (eq (first src) '+))
       (let* ((reg (second src))
              (offset (third src))
              (reg-val (vm-get vm reg)))
         (let ((address (+ reg-val offset)))
           (vm-set vm dest (mem-get vm address)))))

      (t (format t "Erreur: Chargement invalide~%")))
    (format t "Chargement effectué : ~A -> ~A~%" src dest)))


(defun handle-store (vm instr)
  (let ((src (second instr))   ;; Source
        (dest (third instr)))  ;; Destination
    (cond
      ;; Si la source est une expression d'adresse indexée
      ((and (listp dest) (eq (first dest) '+))
        (let ((reg (second dest)) (offset (third dest)))
          (mem-set vm (+ (vm-get vm reg) offset) (vm-get vm src))))

      ((and (listp dest) (eq (first dest) '-))
        (let ((reg (second dest)) (offset (third dest)))
          (mem-set vm (+ (vm-get vm reg) offset) (vm-get vm src))))

      ;; Si la source est une constante (:CONST)
      ((and (listp src) (eq (first src) :CONST))
        (mem-set vm (vm-get vm dest) (second src)))
      
      ((and (symbolp src) (symbolp dest))
        (mem-set vm (vm-get vm dest) (vm-get vm src)))

      ;; Si la source est un registre
      ((and (symbolp src))
        (mem-set vm dest (vm-get vm src)))

      (t (format t "Erreur: Stockage invalide~%")))
    (format t "Stockage effectué : ~A -> ~A~%" src dest)))


(defun handle-move (vm instr)
  (let ((src (second instr))   ;; Source
        (dest (third instr)))  ;; Destination
    (cond
      ;; Si la source est une constante (:CONST)
      ((and (listp src) (eq (first src) :CONST))
       (vm-set vm dest (second src)))

      ;; Si la source est un registre
      ((symbolp src)
       (vm-set vm dest (vm-get vm src)))

      (t (format t "Erreur: Mouvement invalide~%")))
    (format t "Mouvement effectué : ~A -> ~A~%" src dest)))


(defun handle-add (vm instr)
  (let ((reg1 (second instr))
        (reg2 (third instr)))
    (if (listp reg1)
        (vm-set vm reg2 (+ (vm-get vm reg2) (second reg1)))
        (vm-set vm reg2 (+ (vm-get vm reg2) (vm-get vm reg1))))))

        
(defun handle-sub (vm instr)
  (let ((reg1 (second instr))
        (reg2 (third instr)))
    (if (listp reg1)
        (vm-set vm reg2 (- (vm-get vm reg2) (second reg1)))
        (vm-set vm reg2 (- (vm-get vm reg2) (vm-get vm reg1))))))

(defun handle-mul (vm instr)
  (let ((reg1 (second instr))
        (reg2 (third instr)))
    (if (listp reg1)
        (vm-set vm reg2 (* (vm-get vm reg2) (second reg1)))
        (vm-set vm reg2 (* (vm-get vm reg2) (vm-get vm reg1))))))

(defun handle-div (vm instr)
  (let ((reg1 (second instr))
        (reg2 (third instr)))
    (if (listp reg1)
        (vm-set vm reg2 (/ (vm-get vm reg2) (second reg1)))
        (vm-set vm reg2 (/ (vm-get vm reg2) (vm-get vm reg1))))))

(defun handle-incr (vm instr)
  (let ((reg (second instr)))
    (vm-set vm reg (+ (vm-get vm reg) 1)))
)

(defun handle-decr (vm instr)
  (let ((reg (second instr)))
    (vm-set vm reg (- (vm-get vm reg) 1)))
)

(defun handle-push (vm instr)
  (let ((src (second instr)))
    (handle-incr vm (list 'INCR :SP))  ; Increment SP
    (handle-store vm (list 'STORE src :SP))  ; Store src at the new SP address
  )
)

(defun handle-pop (vm instr)
  (let ((dest (second instr)))
    (handle-load vm (list 'LOAD :SP dest))  ; Load the content of SP into dest
    (handle-decr vm (list 'DECR :SP))  ; Decrement SP
  )
)

(defun handle-jmp (vm instr)
  (let ((address (second instr))) ; Assuming the address is directly specified
    (pc-set vm (+ address 1))))

(defun handle-jsr (vm instr)
  (let ((return-address (+ (pc-get vm) 1))) ; Récupère l'adresse de retour
    (handle-push vm (list 'PUSH return-address)) ; Empile l'adresse de retour
    (handle-jmp vm instr) ; Effectue le saut vers le label
  )
)

(defun handle-rtn (vm instr)
  (let ((return-address (vm-get vm :SP))) ; Récupère l'adresse en sommet de pile
    (handle-load vm (list 'LOAD :SP :R0)) ; Charge l'adresse en sommet de pile dans R0
    (handle-decr vm (list 'DECR :SP))    ; Décrémente SP pour simuler le dépilement
    (handle-jmp vm (list 'JMP (vm-get vm :R0))) ; Saut vers l'adresse enregistrée dans R0
  )
)

(defun handle-cmp (vm instr)
  (let ((reg1 (second instr))
        (reg2 (third instr)))
    (let ((val1 (vm-get vm reg1))
          (val2 (vm-get vm reg2)))
      (vm-set vm :FEQ (if (= val1 val2) 1 0))
      (vm-set vm :FLT (if (< val1 val2) 1 0))
      (vm-set vm :FGT (if (> val1 val2) 1 0))))) ; Print the results of comparison


(defun handle-jgt (vm instr)
  (let ((address (second (instr))))
    (when (and address (> (vm-get vm :FGT) 0))
      (pc-set vm (+ address 1)))))

(defun handle-jge (vm instr)
  (let ((address (second (instr))))
    (when (or (and address (> (vm-get vm :FGT) 0)) (= (vm-get vm :FEQ) 1))
      (pc-set vm (+ address 1)))))

(defun handle-jlt (vm instr)
  (let ((address (second (instr))))
    (when (and address (< (vm-get vm :FLT) 0))
      (pc-set vm (+ address 1)))))

(defun handle-jle (vm instr)
  (let ((address (second (instr))))
    (when (or (and address (< (vm-get vm :FLT) 0)) (= (vm-get vm :FEQ) 1))
      (pc-set vm (+ address 1)))))

(defun handle-jeq (vm instr)
  (if (equal (vm-get vm :FEQ) 1)
      (if (listp instr)
          (let ((address (second instr)))
            (format t "Address: ~A~%" address) ; Print the extracted address for debugging
            (pc-set vm (+ address 1))))))

(defun handle-jne (vm instr)
  (if (/= (vm-get vm :FEQ) 1)
      (if (listp instr)
          (let ((address (second instr)))
            (pc-set vm (+ address 1))))))

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
