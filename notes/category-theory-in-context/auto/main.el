(TeX-add-style-hook
 "main"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("report" "14pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("todonotes" "colorinlistoftodos" "prependcaption" "textsize=tiny")))
   (add-to-list 'LaTeX-verbatim-environments-local "minted")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "report"
    "rep10"
    "classicthesis"
    "bbm"
    "bbding"
    "physics"
    "amsmath"
    "amssymb"
    "graphicx"
    "makeidx"
    "algpseudocode"
    "algorithm"
    "listing"
    "minted"
    "cancel"
    "quiver"
    "booktabs"
    "subcaption"
    "enumitem"
    "mathtools"
    "xargs"
    "todonotes"
    "hyperref"
    "epsfig"
    "tabularx"
    "latexsym")
   (TeX-add-symbols
    '("proof" 1)
    '("question" 1)
    '("start" 1)
    '("inv" 1)
    '("subcat" 2)
    '("functor" 3)
    '("cat" 1)
    '("pf" 1)
    '("pushfwd" 1)
    '("pullback" 1)
    '("pushforward" 1)
    '("ddfrac" 2)
    "N"
    "Z"
    "Q"
    "R"
    "coT"
    "Lie"
    "Vectorfield"
    "vectorfield"
    "boldX"
    "boldY"
    "functordef"
    "cc"
    "ee"
    "ccat"
    "cset"
    "opc"
    "opd"
    "ope"
    "mono"
    "epi"
    "bg"
    "bgg"
    "nt"
    "zero"
    "one"
    "two"
    "three"
    "id"
    "G"
    "answer"
    "X"
    "comma"
    "qed")
   (LaTeX-add-environments
    "theorem"
    "corollary"
    "definition"
    "lemma"
    "observation"
    "remark"
    "example"
    "exercise"))
 :latex)

