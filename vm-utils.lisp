(defun vm-get (vm attr)
  (get vm attr))

(defun vm-set (vm attr value)
  (setf (get mv attr) val)) ;; prop c'est attr non?!

(defun get-mem (mv adr)
  (aref (get mv :MEM) adr))

(defun tab-set(tab cle val)
  (setf (aref tab cle) val))

(defun mem-set(mv adr val)
  (tab-set (get mv :MEM) adr val))

(defun pc-get(&optional (mv 'mv))
  (vm-get mv :PC))

(defun pc-set(mv val)
  (vm-set mv :PC val))

(defun pc-inc(mv)
  (pc-set mv (+ (pc-get mv) 1)))

(defun pc-dec(mv) ;;pc-dec?
  (pc-set mv (- (pc-get mv) 1))) ;;pc-get?

(defun lc-get(&optional (mv 'mv))
  (vm-get mv :LC))

(defun lc-set(mv val)
  (vm-set mv :LC val))

(defun lc-inc(mv)
  (lc-set mv (+ (lc-get mv) 1)))

(defun lc-dec(mv)
  (lc-set mv (- (lc-get mv) 1)))