(require "vm/utils/require.lisp")
(require "vm/insn/require.lisp")

;; —————————————   MAX_MEM
;; Zone code (4/5) PC
;; —————————————   MS
;; Zone pile (1/5) SP
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
    (pc-set vm (- size 1))               ;; puis on va diminuer dans la mémoire, ça permet de ne pas trop se faire de soucis
    (bp-set vm 30)                       ;; Le BP lui est défini après les variables basses.
    (sp-set vm (bp-get vm))              ;; Le stack pointer est de base sur BP.
    (ms-set vm (+ 30 (/ tailleZones 5))) ;; s'en suit la valeur maximum du stack qu'on ne doit pas dépasser
    (set-running vm 1)))                 ;; Ainsi pour une VM taille 1000: BP = 30, SP = 30, MS = 224, MAX_MEM = 1000

(defun vm-init(vm &optional (size 1000))
  (attr-set vm :NAME vm)
  (vm-reset vm))

(defun vm-load (vm program)
  ;; Charge un programme dans la mémoire de la machine virtuelle.
    (let ((initial-pc (pc-get vm)))  ;; Sauvegarde la valeur initiale du PC.
        (loop for insn in program do
            (mem-set vm initial-pc insn) ;; Place chaque instruction dans la mémoire.
            (setq initial-pc (- initial-pc 1))  ;; Decrémente la position de la mémoire. 999->998->997
        )
        (attr-set vm :LAST_CODE (+ initial-pc 1)) ;; on a enregistré l'adress de la dernière instruction! et le début 
        (pc-set vm (pc-get vm))   ;; Réinitialise le PC à la position initiale.
    )
)



(defun vm-execute (vm)
  (loop while (>= (pc-get vm) (attr-get vm :LAST_CODE)) do
    (let ((insn (mem-get vm (pc-get vm))))
      (cond
        ((equal (first insn) 'LOAD) (handle-load vm insn))
        ((equal (first insn) 'STORE) (handle-store vm insn))
        ((equal (first insn) 'MOVE) (handle-move vm insn))
        (t (format t "Instruction inconnue: ~A~%" insn)))
      (pc-decr vm))))