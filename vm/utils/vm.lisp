(require "vm/utils/attr.lisp")

(defun pc-get(vm)
  (attr-get vm :PC))

(defun pc-set(vm val)
  (attr-set vm :PC val))

(defun pc-incr(vm)
  (pc-set vm (+ (pc-get vm) 1)))

(defun pc-decr(vm)
  (pc-set vm (- (pc-get vm) 1)))

(defun bp-get(vm)
  (attr-get vm :BP))

(defun bp-set(vm val)
  (attr-set vm :BP val))

(defun sp-get(vm)
  (attr-get vm :SP))

(defun sp-set(vm val)
  (attr-set vm :SP val))

(defun ms-get(vm)
  (attr-get vm :MS))

(defun ms-set(vm val)
  (attr-set vm :MS val))

(defun is-running(vm)
  (equal (attr-get vm :RUNNING) 1))

(defun set-running(vm val)
  (attr-set vm :RUNNING val))

(defun is-vm-attr(val)
  (if (not (listp val))
    (let ((attributes '("R0" "R1" "R2" "SP" "BP" "PC" "MS" "FP" "FEQ" "FLT" "FGT")))
      (loop for attr in attributes
            when (equal (string val) attr) do (return t)
            finally (return nil)))))

(defun to-vm-attr (val)
  (cond
    ((equal (string val) "R0") :R0)
    ((equal (string val) "R1") :R1)
    ((equal (string val) "R2") :R2)
    ((equal (string val) "SP") :SP)
    ((equal (string val) "BP") :BP)
    ((equal (string val) "PC") :PC)
    ((equal (string val) "MS") :MS)
    ((equal (string val) "FP") :FP)
    ((equal (string val) "FEQ") :FEQ)
    ((equal (string val) "FLT") :FLT)
    ((equal (string val) "FGT") :FGT)
    (t (format t "Attribut inconnu: ~A~%" val))))

(defun is-jmp (insn)
  (if (member (first insn) '(JMP JSR JGT JGE JLT JLE JEQ JNE JTRUE JNIL))
      t
      nil))

(defun is-label(insn)
  (equal (first insn) 'LABEL))