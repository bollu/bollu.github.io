.PHONY: view
view: main.pdf
	okular -unique main.pdf
main.pdf: main.tex
	latexmk main.tex -shell-escape -pdf
