.PHONY: main.pdf

main.pdf: *.tex
	latexmk -pdf main.tex && latexmk -c

term-paper-final.pdf: term-paper-final.tex term-paper-final.bib
	latexmk -pdf term-paper-final.tex && latexmk -c
term-paper-mid.pdf: term-paper-mid.tex term-paper-mid.bib
	latexmk -pdf term-paper-mid.tex && latexmk -c


watch: *.tex
	latexmk -pdf -pvc main.tex && latexmk -c
