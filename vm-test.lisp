(require "vm.lisp")

(let ((my-vm (make-hash-table))) ; Assuming 'make-hash-table' creates a new VM object
  (vm-load my-vm 2000)) ; Call vm-make, passing 'my-vm' as the VM object and '2000' as the size