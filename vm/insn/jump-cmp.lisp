(defun handle-jmp (vm insn)
  (let ((target (second insn)))
    (if (numberp target)
        (pc-set vm (+ target 1))
        (pc-set vm (+ (attr-get vm (to-vm-attr target)) 1)))))

(defun handle-jsr (vm insn)
  ;; Extraire l'étiquette de l'instruction
  (let ((label (second insn)))
    (if (or (numberp label) (is-etiq-set vm label))
        (progn
          (attr-set vm :R1 (- (pc-get vm) 1))
          (handle-push vm '(PUSH R1))
          (handle-jmp vm insn))
        (error "Etiquette non définie: ~a" label))))

(defun handle-cmp (vm insn)
  (let ((reg1 (second insn))
        (reg2 (third insn)))
    (let ((val1 (cond 
            ((is-const reg1) (second reg1))
            ((is-vm-attr reg1) (attr-get vm (to-vm-attr reg1)))))
          (val2 (attr-get vm (to-vm-attr reg2))))
      (attr-set vm :FEQ (if (= val1 val2) 1 0))
      (attr-set vm :FLT (if (< val1 val2) 1 0))
      (attr-set vm :FGT (if (> val1 val2) 1 0)))))

(defun handle-jgt (vm insn)
  (if (eq (attr-get vm :FGT) 1)
      (handle-jmp vm insn)))

(defun handle-jge (vm insn)
  (if (or (eq (attr-get vm :FGT) 1) (eq (attr-get vm :FEQ) 1))
      (handle-jmp vm insn)))

(defun handle-jlt (vm insn)
  (if (eq (attr-get vm :FLT) 1)
      (handle-jmp vm insn)))

(defun handle-jle (vm insn)
  (if (or (eq (attr-get vm :FLT) 1) (eq (attr-get vm :FEQ) 1))
      (handle-jmp vm insn)))

(defun handle-jeq (vm insn)
  (if (eq (attr-get vm :FEQ) 1)
      (handle-jmp vm insn)))

(defun handle-jne (vm insn)
  (if (eq (attr-get vm :FEQ) 0)
      (handle-jmp vm insn)))

(defun handle-test(vm insn)
  (let ((dst (second insn)))
    (let ((v (cond 
            ((is-const dst) (second dst))
            ((is-vm-attr dst) (attr-get vm (to-vm-attr dst))))))
      (attr-set vm :FNIL (null v)))))

(defun handle-jtrue (vm insn)
  (if (not (attr-get vm :FNIL))
    (handle-jmp vm insn)))

(defun handle-jnil (vm insn)
  (if (attr-get vm :FNIL)
    (handle-jmp vm insn)))