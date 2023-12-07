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
  (= (attr-get vm :RUNNING) 1))

(defun set-running(vm val)
  (attr-set vm :RUNNING val))