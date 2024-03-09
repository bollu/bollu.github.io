.PHONY: main.pdf
main.pdf: *.tex
	pdflatex -interaction=nonstopmode  -halt-on-error -shell-escape main.tex

watch: *.tex
	latexmk -pvc  -shell-escape main.tex
