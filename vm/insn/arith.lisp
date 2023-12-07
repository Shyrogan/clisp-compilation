(defun handle-add(vm insn)
  (let ((src (second insn)) (dst (third insn)))
    (cond
      ((is-const src) (attr-set vm (to-vm-attr dst)
        (+ (attr-get vm (to-vm-attr dst)) (second src))))
      ((is-vm-attr src) (attr-set vm (to-vm-attr dst)
        (+ (attr-get vm (to-vm-attr dst)) (attr-get vm (to-vm-attr src))))))))

(defun handle-sub(vm insn)
  (let ((src (second insn)) (dst (third insn)))
    (cond
      ((is-const src) (attr-set vm (to-vm-attr dst)
        (- (attr-get vm (to-vm-attr dst)) (second src))))
      ((is-vm-attr src) (attr-set vm (to-vm-attr dst)
        (- (attr-get vm (to-vm-attr dst)) (attr-get vm (to-vm-attr src))))))))

(defun handle-mul(vm insn)
  (let ((src (second insn)) (dst (third insn)))
    (cond
      ((is-const src) (attr-set vm (to-vm-attr dst)
        (* (attr-get vm (to-vm-attr dst)) (second src))))
      ((is-vm-attr src) (attr-set vm (to-vm-attr dst)
        (* (attr-get vm (to-vm-attr dst)) (attr-get vm (to-vm-attr src))))))))

(defun handle-div(vm insn)
  (let ((src (second insn)) (dst (third insn)))
    (cond
      ((is-const src) (attr-set vm (to-vm-attr dst)
        (/ (attr-get vm (to-vm-attr dst)) (second src))))
      ((is-vm-attr src) (attr-set vm (to-vm-attr dst)
        (/ (attr-get vm (to-vm-attr dst)) (attr-get vm (to-vm-attr src))))))))

(defun handle-incr(vm insn)
  (if (is-vm-attr (second insn)) (attr-set vm (to-vm-attr (second insn)) (+ (attr-get vm (to-vm-attr (second insn))) 1))))

(defun handle-decr(vm insn)
  (if (is-vm-attr (second insn)) (attr-set vm (to-vm-attr (second insn)) (- (attr-get vm (to-vm-attr (second insn))) 1))))