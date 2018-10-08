.PHONY: main.pdf
main.pdf: *.tex
	latexmk -pdf -shell-escape main.tex && latexmk -c

watch: *.tex
	# latexmk -pvc -pdf -shell-escape main.tex
	latexmk -pvc  -shell-escape main.tex
