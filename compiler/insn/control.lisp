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
     (comp condition)
     `((CMP (:CONST 0) R0) (JEQ ,etiq-else))
     (comp then-part)
     `((JMP ,etiq-end))
     `((LABEL ,etiq-else))
     (when else-part (comp else-part))
     `((LABEL ,etiq-end)))))

(defun comp-while (expr)
  (let ((test (first expr))
        (body (second expr))
        (etiq-boucle (generate-label))
        (etiq-fin (generate-label)))
    (append
     `((LABEL ,etiq-boucle))
     (comp test)
     `((CMP (:CONST 0) R0) (JEQ R0 ,etiq-fin))
     (comp body)
     `((JMP ,etiq-boucle))
     `((LABEL ,etiq-fin)))))

(defun comp-defun (expr)
  (let ((proc (second expr))
        (body (third expr)))
    (append
     `((LABEL ,proc))
     (comp body)
     '((RTN)))))