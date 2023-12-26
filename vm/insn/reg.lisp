(defun handle-move(vm insn)
  (let ((src (second insn)) (dst (third insn)))
    (cond
      ((is-const src) (attr-set vm (to-vm-attr dst) (second src)))
      ((is-global-var src) (attr-set vm (to-vm-attr dst) (attr-map-get vm :ETIQ (second src))))
      ((is-vm-attr src) (attr-set vm (to-vm-attr dst) (attr-get vm (to-vm-attr src)))))))