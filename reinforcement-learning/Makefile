.PHONY: main.pdf
main.pdf: *.tex
	latexmk -pdf -shell-escape main.tex && latexmk -c

watch: *.tex
	latexmk -pdf  -shell-escape -pvc main.tex && latexmk -c
