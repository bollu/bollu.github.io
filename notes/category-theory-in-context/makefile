.PHONY: view
	
calculus-of-adjunctions.pdf: calculus-of-adjunctions.tex
	latexmk calculus-of-adjunctions.tex -shell-escape -pdf

view: main.pdf
	okular -unique main.pdf
main.pdf: main.tex
	latexmk main.tex -shell-escape -pdf
