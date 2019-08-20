.PHONY: main.pdf
main.pdf: *.tex
	latexmk -pdf main.tex && latexmk -c

watch: *.tex
	latexmk -pdf -pvc main.tex && latexmk -c
