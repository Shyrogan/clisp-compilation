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

(defun comp-while (expr)
  ;; expr est de la forme (while test body)
  (let ((test (first expr))
        (body (second expr))
        (etiq-boucle (generate-label))
        (etiq-fin (generate-label)))
    (append
     `((LABEL ,etiq-boucle))
     (comp test)
     `((CMP (:CONST 0) R0) (JEQ ,etiq-fin))
     (comp body)
     `((JMP ,etiq-boucle))
     `((LABEL ,etiq-fin)))))

(defun comp-for (expr)
  ;; expr est de la forme (for init condition increment body)
  (let ((init (first expr))
        (condition (second expr))
        (increment (third expr))
        (body (fourth expr))
        (etiq-boucle (generate-label))
        (etiq-fin (generate-label)))
    (append
     ;; Initialisation
     (comp init)
     ;; Étiquette de début de boucle
     `((LABEL ,etiq-boucle))
     ;; Compilation de la condition
     (comp condition)
     ;; Sauter à la fin si la condition est fausse
     `((CMP (:CONST 0) R0) (JEQ ,etiq-fin))
     ;; Compilation du corps de la boucle
     (comp body)
     ;; Compilation de l'incrément
     (comp increment)
     ;; Retourner au début de la boucle
     `((JMP ,etiq-boucle))
     ;; Étiquette de fin de boucle
     `((LABEL ,etiq-fin)))))
