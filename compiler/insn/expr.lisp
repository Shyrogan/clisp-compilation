(defun comp-const (expr ctx)
  `((MOVE (:CONST ,expr) R0) (PUSH R0)))

(defun comp-var (var ctx)
  (let ((local-offset (cdr (assoc var ctx))))
    (if local-offset
        ;; Charger une variable locale en utilisant son offset
        `((LOAD (+ FP ,local-offset) R0) (PUSH R0))
        ;; Charger une variable globale
        `((LOAD (:@ ,var) R0) (PUSH R0)))))

(defun comp-setf(operands ctx)
  ;; operands est de la forme (variable valeur)
  (let ((variable (string (first operands)))  ; Le nom de la variable
        (valeur (second operands)))   ; La valeur à assigner
    (append
     (comp valeur)
     `((STORE R0 (:@ ,variable)))))) 

(defun extend-context-with-bindings (bindings ctx)
  (let ((extended-ctx ctx)
        (offset (+ (length ctx) 1)))  ; Détermine la position de départ pour les nouvelles variables
    (dolist (binding bindings extended-ctx)
      (let ((var-name (first binding)))
        (setq extended-ctx (cons (cons var-name offset) extended-ctx))
        (incf offset)))))  ; Augmente l'offset pour la prochaine variable

(defun comp-let (bindings body ctx)
  (let ((new-ctx (extend-context-with-bindings bindings ctx))
        (binding-instrs '())
        (cleanup-instrs '()))
    (setq binding-instrs (append '(
      (PUSH FP)
      (MOVE SP FP)
    ) binding-instrs))

    (dolist (binding bindings)
      (setq binding-instrs (append binding-instrs
                                   (comp (second binding) ctx))))
                                  
    (let ((body-instrs (comp body new-ctx)))
      (dotimes (i (length bindings))
        (setq cleanup-instrs (append '((POP R1)) cleanup-instrs)))
      (setq cleanup-instrs (append '((POP FP)) cleanup-instrs))

      (append binding-instrs body-instrs cleanup-instrs))))
