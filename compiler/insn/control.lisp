(defun comp-seq (expressions)
  (let ((instructions '()))
    ;; Parcourir chaque expression dans la séquence
    (dolist (expr expressions instructions)
      ;; Compiler l'expression et ajouter ses instructions à la liste
      (setq instructions (append instructions (comp expr)))
    )
  )
)

(defun comp-if (expr)
  ;; expr est de la forme (if condition then [else])
  (let ((condition (first expr))
        (then-part (second expr))
        (else-part (third expr))  ; Peut être nil si le 'else' est absent
        (etiq-else (generate-label))
        (etiq-end (generate-label)))
    (append
     ;; Compile la condition
     (comp condition)
     ;; Compare le résultat et saute à etiq-else si faux
     `((CMP (:CONST 0) R0) (JEQ ,etiq-else))
     ;; Compile la branche 'then'
     (comp then-part)
     ;; Saute à la fin après le bloc 'then'
     `((JMP ,etiq-end))
     ;; Étiquette et bloc 'else'
     `((LABEL ,etiq-else))
     (when else-part (comp else-part))  ; Compile le bloc 'else' s'il existe
     ;; Étiquette de fin
     `((LABEL ,etiq-end)))))