(defun extend-context-with-params(params ctx)
  (let ((offset (length ctx)))
    (dolist (param params ctx)
      (setq ctx (cons (cons param (- offset 3)) ctx))
      (decf offset))))

(defun comp-fun (fun-name params body ctx)
  (let ((new-ctx (extend-context-with-params params ctx))
        (entry-label (string fun-name))
        (exit-label (concatenate 'string (string fun-name) "-exit"))
        (cleanup-instrs '()))

    ;; Assemblage des instructions de la fonction
    (append `((JMP ,exit-label)
            (LABEL ,entry-label))
            `((PUSH FP) (MOVE SP FP))
            (comp body new-ctx)
            cleanup-instrs
            `((POP R0) (POP FP) (POP R1) (JMP R1))
            `((LABEL ,exit-label)))))

(defun comp-call (fun-name args ctx)
  (let ((call-instrs '()))
    ;; Compiler les arguments et les ajouter à call-instrs
    (dolist (arg args)
      (setq call-instrs (append call-instrs (comp arg ctx))))
    ;; Ajouter le nombre d'arguments
    (let ((nbArg (length args)))
      (setq call-instrs (append call-instrs `((MOVE (:CONST ,nbArg) R0) (PUSH R0)))))

    ;; Ajouter l'adresse de retour et l'instruction JSR à call-instrs
    (setq call-instrs (append call-instrs `((JSR ,fun-name))))

    (dolist (arg args)
      (setq call-instrs (append call-instrs '((POP R1)))))
    (setq call-instrs (append call-instrs `((POP R1))))
    (setq call-instrs (append call-instrs '((PUSH R0))))

    ;; Retourner les instructions de l'appel de fonction
    call-instrs))
