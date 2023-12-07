(require "vm/utils/array.lisp")

(defun mem-get(vm key)
  (attr-array-get vm :MEM key))

(defun mem-set(vm key val)
  (attr-array-set vm :MEM key val))

(defun is-const(val)
  (and (listp val) (equal (first val) :CONST)))

(defun is-offset(val)
  (and (listp val) (equal (string (first val)) '"+") (is-vm-attr (second val))))
