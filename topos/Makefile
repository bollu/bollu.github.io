.PHONY: main.pdf
main.pdf: *.tex
	latexmk -pdf -shell-escape main.tex && latexmk -c

watch: *.tex
	latexmk -shell-escape -pdf -pvc main.tex && latexmk -c
