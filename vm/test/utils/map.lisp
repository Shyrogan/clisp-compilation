(require "vm/utils/map.lisp")

;; Vérifie que array-set/get, attr-array-set/get fonctionne
(let ((vm '()) (label 'HELLO))
  (attr-map-init vm :ETIQ 5)
  (attr-map-set vm :ETIQ label 300)
  (format t "Test ATTR-MAP-SET, ATTR-MAP-GET: ~A~%" (= (attr-map-get vm :ETIQ label) 300)))