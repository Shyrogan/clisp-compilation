(require "vm/utils/require.lisp")

(defun handle-load(vm insn)
  (let ((src (second insn)) (dst (third insn)))
    (cond
      ((numberp src) (attr-set vm (to-vm-attr dst)(mem-get vm src)))
      ((is-vm-attr src) (attr-set vm (to-vm-attr dst) (mem-get vm (attr-get vm (to-vm-attr src)))))
      ((is-offset src)
        (let ((offset (third src)) (attr (to-vm-attr (second src))))
          (attr-set vm (to-vm-attr dst) (mem-get vm (+ (attr-get vm attr) offset)))))
      (t (format t "La source doit-être soit un nombre, soit un registre, soit un offset: ~A~%" insn))
    )))

(defun handle-store (vm insn)
  (let ((src (second insn)) (dst (third insn)))
    (let ((srcMapped (cond
                      ((is-const src) (second src))
                      ((is-vm-attr src) (attr-get vm (to-vm-attr src)))
                      (t (format t "La source doit-être soit une constante, soit un registre: ~A~%" insn))))
          (dstMapped (cond
                      ((numberp dst) dst)
                      ((is-vm-attr dst) (attr-get vm (to-vm-attr dst)))
                      ((is-offset dst) (+ (third dst) (attr-get vm (to-vm-attr (second dst)))))))
          (isGlobalVar (is-global-var dst)))
      (if isGlobalVar
          (etiq-set vm (second dst) srcMapped)
          (mem-set vm dstMapped srcMapped)))))