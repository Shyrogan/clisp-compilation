(defun perform-arithmetic-op (vm src dst op)
  (let ((src-value (if (is-const src) (second src) (attr-get vm (to-vm-attr src))))
        (dst-value (attr-get vm (to-vm-attr dst))))
    (attr-set vm (to-vm-attr dst) (funcall op dst-value src-value))))

(defun handle-add (vm insn)
  (perform-arithmetic-op vm (second insn) (third insn) #'+))

(defun handle-sub (vm insn)
  (perform-arithmetic-op vm (second insn) (third insn) #'-))

(defun handle-mul (vm insn)
  (perform-arithmetic-op vm (second insn) (third insn) #'*))

(defun handle-div (vm insn)
  (perform-arithmetic-op vm (second insn) (third insn) #'/))

(defun handle-incr-decr (vm insn op)
  (let ((attr (to-vm-attr (second insn))))
    (if (is-vm-attr attr)
        (attr-set vm attr (funcall op (attr-get vm attr) 1)))))

(defun handle-incr (vm insn)
  (handle-incr-decr vm insn #'+))

(defun handle-decr (vm insn)
  (handle-incr-decr vm insn #'-))
