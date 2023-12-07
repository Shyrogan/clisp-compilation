(require "vm/utils/require.lisp")
(require "vm/insn/require.lisp")

;; —————————————   MAX_MEM
;; Zone code (1/2) PC
;; —————————————   MS
;; Zone pile (1/2) SP
;; —————————————   BP
;; Variables basses (30)
;; ————————————— 0
(defun vm-reset(vm &optional (size 1000))
  (let ((size (max size 1000)) (variablesBasse 30) (tailleZones (- (max size 1000) 30)))
    (attr-set vm :R0 0)
    (attr-set vm :R1 0)
    (attr-set vm :R2 0)
    (attr-set vm :R3 0)
    (attr-set vm :MAX_MEM size)          ;; Définition de la taille de la VM
    (attr-array-init vm :MEM size)       ;; Définition de la mémoire
    (attr-set vm :START_CODE (- size 1)) ;; Les instructions du code commence en haut de la mémoire,
    (attr-set vm :LAST_CODE (- size 1)) ;; Les instructions du code commence en haut de la mémoire,
    (attr-map-init vm :ETIQ 100)
    (pc-set vm (- size 1))               ;; puis on va diminuer dans la mémoire, ça permet de ne pas trop se faire de soucis
    (bp-set vm 30)                       ;; Le BP lui est défini après les variables basses.
    (sp-set vm (bp-get vm))              ;; Le stack pointer est de base sur BP.
    (ms-set vm (+ 30 (/ tailleZones 2))) ;; s'en suit la valeur maximum du stack qu'on ne doit pas dépasser
    (set-running vm 1)))                 ;; Ainsi pour une VM taille 1000: BP = 30, SP = 30, MS = 224, MAX_MEM = 1000

(defun vm-init(vm &optional (size 1000))
  (attr-set vm :NAME vm)
  (vm-reset vm))

(defun vm-load (vm program)
    (let ((initial-pc (pc-get vm)))
        ;; Première itération sur le programme
        (loop for insn in program do
            (if (is-label insn)
              ;; Si c'est un label, on le stock dans le registre à label
              (progn
                (attr-map-set vm :ETIQ (string (second insn)) initial-pc))
              ;; Sinon, on le met en mémoire et décrémente PC
              (progn 
                (mem-set vm initial-pc insn)
                (setq initial-pc (- initial-pc 1)))))
        (setq initial-pc (pc-get vm))
        (loop for insn in program do
          ;; Remplace les sauts par des sauts vers des adresses
          (if (is-jmp insn)
            (mem-set vm initial-pc (list (first insn) (attr-map-get vm :ETIQ (string (second insn))))))
          ;; On décrémente uniquement si ce n'est pas un label
          (if (not (is-label insn))
            (setq initial-pc (- initial-pc 1))))
        (attr-set vm :LAST_CODE (+ initial-pc 1))
        (pc-set vm (pc-get vm))
    )
)

(defun vm-execute (vm)
  (loop while (and (>= (pc-get vm) (attr-get vm :LAST_CODE)) (is-running vm)) do
    (let ((insn (mem-get vm (pc-get vm))))
      (cond
        ((equal (first insn) 'LOAD) (handle-load vm insn))
        ((equal (first insn) 'STORE) (handle-store vm insn))
        ((equal (first insn) 'MOVE) (handle-move vm insn))
        ((equal (first insn) 'ADD) (handle-add vm insn))
        ((equal (first insn) 'SUB) (handle-sub vm insn))
        ((equal (first insn) 'MUL) (handle-mul vm insn))
        ((equal (first insn) 'DIV) (handle-div vm insn))
        ((equal (first insn) 'INCR) (handle-incr vm insn))
        ((equal (first insn) 'DECR) (handle-decr vm insn))
        ((equal (first insn) 'PUSH) (handle-push vm insn))
        ((equal (first insn) 'POP) (handle-pop vm insn))
        ((equal (first insn) 'NOP) (handle-nop vm insn))
        ((equal (first insn) 'HALT) (handle-halt vm insn))
        ((equal (first insn) 'JMP) (handle-jmp vm insn))
        ((equal (first insn) 'CMP) (handle-cmp vm insn))
        ((equal (first insn) 'JSR) (handle-jsr vm insn))
        ((equal (first insn) 'JGT) (handle-jgt vm insn))
        ((equal (first insn) 'JGE) (handle-jge vm insn))
        ((equal (first insn) 'JLT) (handle-jlt vm insn))
        ((equal (first insn) 'JLE) (handle-jle vm insn))
        ((equal (first insn) 'JEQ) (handle-jeq vm insn))
        ((equal (first insn) 'JNE) (handle-jne vm insn))
        ((equal (first insn) 'TEST) (handle-test vm insn))
        ((equal (first insn) 'JNIL) (handle-jnil vm insn))
        ((equal (first insn) 'JTRUE) (handle-jtrue vm insn))
        (t (format t "Instruction inconnue: ~A~%" insn)))
      (pc-decr vm))))