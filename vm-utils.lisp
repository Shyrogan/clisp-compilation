(defun vm-get (vm attr)
  (get vm attr))

(defun vm-set (vm attr val)
  (setf (get vm attr) val)) ;; prop c'est attr non?!

(defun tab-set(tab cle val)
  (setf (aref tab cle) val))

(defun mem-set(vm adr val)
  (tab-set (get vm :MEM) adr val))

(defun mem-get (vm adr)
  (aref (get vm :MEM) adr))

(defun pc-get(&optional (vm 'vm))
  (vm-get vm :PC))

(defun pc-set(vm val)
  (vm-set vm :PC val))

(defun pc-inc(vm)
  (pc-set vm (+ (pc-get vm) 1)))

(defun pc-dec(vm) ;;pc-dec?
  (pc-set vm (- (pc-get vm) 1))) ;;pc-get?

(defun lc-get(&optional (vm 'vm))
  (vm-get vm :LC))

(defun lc-set(vm val)
  (vm-set vm :LC val))

(defun lc-inc(vm)
  (lc-set vm (+ (lc-get vm) 1)))

(defun lc-dec(vm)
  (lc-set vm (- (lc-get vm) 1)))



  #| Elements de test |#
(defun afficher-registres (vm)
  (loop for (reg val) on vm by #'cddr
        do (format t "~A: ~A~%" reg val)
  )
)