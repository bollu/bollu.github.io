.PHONY: main.pdf

term-paper-mid.pdf: *.tex
	latexmk -pdf term-paper-mid.tex && latexmk -c

main.pdf: *.tex
	latexmk -pdf main.tex && latexmk -c

watch: *.tex
	latexmk -pdf -pvc main.tex && latexmk -c
