(require "compiler/compiler.lisp")


(print (optimize-expr '( let (( i 3 )) (if (< x 4) (+ 10 (+ 2 3 )) (+ 2 3)))))


(print (optimize-expr '(if nil (+ 10 (+ 2 3)) (+ 2 3))))

(print (optimize-expr '(- (* 15 10) (/ 100 10)) ))

(print (optimize-expr '(>= 3 1) ))


(print (optimize-expr '(>= 3 1) ))


(print (optimize-expr '(or (>= 15 10) (not (> x 2)) ) ))

(print (optimize-expr '(and (>= x 10) (< 5 2))))

(print (optimize-expr '( not nil) ) )

(print (optimize-expr '(or (not (>= 15 10)) (not (>= 15 10) ) ) ))

