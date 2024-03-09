.PHONY: main.pdf zip 20161105.pdf

20161105.pdf: 20161105.tex
	latexmk -pdf -shell-escape 20161105.tex && latexmk -c


zip: 20161105.pdf 
	zip -9 20161105.zip 20161105.pdf

main.pdf: *.tex
	latexmk -pdf -shell-escape main.tex && latexmk -c

watch: *.tex
	latexmk -pdf -pvc main.tex && latexmk -c
