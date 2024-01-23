(defun comp-seq (expressions ctx)
  (let ((instructions '()))
    ;; Parcourir chaque expression dans la séquence
    (dolist (expr expressions instructions)
      ;; Compiler l'expression et ajouter ses instructions à la liste
      (setq instructions (append instructions (comp expr)))
    )
  )
)

(defun comp-if (expr ctx)
  ;; expr est de la forme (if condition then [else])
  (let ((condition (first expr))
        (then-part (second expr))
        (else-part (third expr))  ; Peut être nil si le 'else' est absent
        (etiq-else (generate-label))
        (etiq-end (generate-label)))
    (append
     ;; Compile la condition
     (comp condition ctx)
     ;; Compare le résultat et saute à etiq-else si faux
     `((CMP (:CONST nil) :R0) (JEQ ,etiq-else))
     ;; Compile la branche 'then'
     (comp then-part ctx)
     ;; Saute à la fin après le bloc 'then'
     `((JMP ,etiq-end))
     ;; Étiquette et bloc 'else'
     `((LABEL ,etiq-else))
     (when else-part (comp else-part ctx))  ; Compile le bloc 'else' s'il existe
     ;; Étiquette de fin
     `((LABEL ,etiq-end)))))

(defun comp-while (expr ctx)
  ;; expr est de la forme (while test body)
  (let ((test (first expr))
        (body (second expr))
        (etiq-boucle (generate-label))
        (etiq-fin (generate-label)))
    (append
     `((LABEL ,etiq-boucle))
     (comp test ctx)
     `((CMP (:CONST nil) :R0) (JEQ ,etiq-fin))
     (comp body ctx)
     `((JMP ,etiq-boucle))
     `((LABEL ,etiq-fin)))))

(defun comp-for (expr ctx)
  ;; expr est de la forme (for init condition increment body)
  (let ((init (first expr))
        (condition (second expr))
        (increment (third expr))
        (body (fourth expr))
        (etiq-boucle (generate-label))
        (etiq-fin (generate-label)))
    (append
     ;; Initialisation
     (comp init ctx)
     ;; Étiquette de début de boucle
     `((LABEL ,etiq-boucle))
     ;; Compilation de la condition
     (comp condition ctx)
     ;; Sauter à la fin si la condition est fausse
     `((CMP (:CONST nil) :R0) (JEQ ,etiq-fin))
     ;; Compilation du corps de la boucle
     (comp body ctx)
     ;; Compilation de l'incrément
     (comp increment ctx)
     ;; Retourner au début de la boucle
     `((JMP ,etiq-boucle))
     ;; Étiquette de fin de boucle
     `((LABEL ,etiq-fin)))))

(defun comp-cond (expr ctx)
  (let ((clauses expr)
        (etiq-end (generate-label))
        (instructions '()))

    (dolist (clause clauses instructions)
      (let ((condition (first clause))
            (body (second clause))
            (etiq-next (generate-label)))
        (if (equal condition 't)
            ;; Si la condition est "t", compiler directement le corps
            (setq instructions (append instructions (comp body ctx) `((JMP ,etiq-end))))
            ;; Sinon, compiler la condition et vérifier si elle est vraie
            (progn
              (setq instructions (append instructions (comp condition ctx)))
              (setq instructions (append instructions `((CMP (:CONST nil) :R0) (JEQ ,etiq-next))))
              (setq instructions (append instructions (comp body ctx) `((JMP ,etiq-end))))
              (setq instructions (append instructions `((LABEL ,etiq-next))))
            ))))
    (append instructions `((LABEL ,etiq-end)))
  )
)

(defun comp-when (expr ctx)
  ;; expr est de la forme (when condition body)
  (let ((condition (first expr))
        (body (rest expr))
        (etiq-end (generate-label)))
    (append
     ;; Compiler la condition
     (comp condition ctx)
     ;; Si la condition est fausse, sauter à l'étiquette de fin
     `((CMP (:CONST nil) :R0) (JEQ ,etiq-end))
     ;; Compiler le corps si la condition est vraie
     (comp-seq body ctx)
     ;; Étiquette de fin
     `((LABEL ,etiq-end)))))