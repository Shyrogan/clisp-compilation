(require "vm/vm.lisp")
(require "compiler/compiler.lisp")

(format t "
Bienvenue dans notre librairie pour la compilation et la virtualisation en Lisp. Voici les fonctions disponibles:

- (vm-init <vm> <taille (optionnel)>) Initialise la machine virtuelle dans l'argument VM.
- (vm-reset <vm>) Réinitialise la machine virtuelle.
- (vm-load <vm> <programme>) Charge un programme au sein de la machine virtuelle (plusieurs peuvent-être chargé à la suite!)
- (vm-execute <vm>) Execute la machine virtuelle.

- (comp <expression lisp>) Compile une expression Lisp en instruction pour la machine virtuelle.
")