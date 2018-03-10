all: *.tex
	latexmk -pdf -c main.tex
	
watch: *.tex
	latexmk -pdf -pvc main.tex && latexmk -c
