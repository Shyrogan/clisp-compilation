(require "compiler/compiler.lisp")


(print (optimize-expr '( let (( i 3 )) (if (< x 4) (+ 10 (+ 2 3 )) (+ 2 3)))))

(print (optimize-expr '(- (* 15 10) (/ 100 10)) ))

(print (optimize-expr '(>= 3 1) ))

