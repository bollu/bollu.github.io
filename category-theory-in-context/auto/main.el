(TeX-add-style-hook
 "main"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("book" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("mathpazo" "sc" "osf") ("eulervm" "euler-digits" "small") ("todonotes" "colorinlistoftodos" "prependcaption" "textsize=tiny")))
   (add-to-list 'LaTeX-verbatim-environments-local "minted")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "book"
    "bk11"
    "mathpazo"
    "eulervm"
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
    "xargs"
    "todonotes"
    "hyperref"
    "epsfig"
    "tabularx"
    "latexsym")
   (TeX-add-symbols
    '("beginproof" ["argument"] 0)
    '("question" 1)
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
    "G"
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

