(require "vm/utils/array.lisp")

(defun mem-get(vm key)
  (attr-array-get vm :MEM key))

(defun mem-set(vm key val)
  (attr-array-set vm :MEM key val))

(defun is-const(val)
  (and (listp val) (equal (first val) :CONST)))

(defun is-offset(val)
  (and (listp val) (equal (string (first val)) '"+") (is-vm-attr (second val))))

(defun is-vm-attr(val)
  (if (not (listp val))
    (let ((attributes '("R0" "R1" "R2" "R3" "SP" "BP" "PC" "MS" "FP")))
      (loop for attr in attributes
            when (equal (string val) attr) do (return t)
            finally (return nil)))))

(defun to-vm-attr (val)
  (cond
    ((equal (string val) "R0") :R0)
    ((equal (string val) "R1") :R1)
    ((equal (string val) "R2") :R2)
    ((equal (string val) "R3") :R3)
    ((equal (string val) "SP") :SP)
    ((equal (string val) "BP") :BP)
    ((equal (string val) "PC") :PC)
    ((equal (string val) "MS") :MS)
    ((equal (string val) "FP") :FP)
    (t (format t "Attribut inconnu: ~A~%" val))))