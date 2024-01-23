// Get Polylux from the official package repository
#import "@preview/polylux:0.3.1": *

// Make the paper dimensions fit for a presentation and the text larger
#set page(paper: "presentation-16-9", numbering: "1/1")
#set text(size: 16pt)

// Use #polylux-slide to create a slide and style it using your favourite Typst functions
#polylux-slide[
  #align(horizon + center)[
    = Présentation de la VM & Compilateur LISP
    
    #v(1cm)

    #image("assets/clisp.svg", width: 100pt)

    #v(1cm)

    #grid(
      columns: 3,
      gutter: 20pt,
      [VIAL Sébastien \ #link("sebastien.vial@etu.umontpellier.fr")],
      [EL JAAFARI Samy \ #link("samy.el-jaafari@etu.umontpellier.fr")],
      [ALMALLOUHI Mohamad Satea \ #link("mohamad-satea.almallouhi@etu.umontpellier.fr")]
    )

    #v(1cm)

    23 janvier 2024
  ]
]

#polylux-slide[
  == Objectifs:

  Tout d'abord en ce qui concerne la *machine virtuelle*:
  1. Être capable d'exécuter les instructions simples
  2. Gérer les instructions plus complexes (gestion des offsets dans les load, des constantes dans d'ordres instructions, ...)
  3. Effectuer tout ça sans planter, en proposant des moyens de débuguer ainsi qu'en optimisant le plus possible les opérations.

  Et ensuite pour le *compilateur*:
  1. Compiler des expressions simples (arithmétiques, comparaisons)
  2. Gérer les opérateurs de contrôle (if, cond, when, for, while)
  3. Compiler des fonctions sans paramètres
  4. Gérer les paramètres à l'aide de la pile
]