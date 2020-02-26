.PHONY: main.pdf

assign1.pdf: assign1.tex
	latexmk -pdf -shell-escape assign1.tex && latexmk -c

main.pdf: *.tex
	latexmk -pdf -shell-escape main.tex && latexmk -c

watch: *.tex
	latexmk -pdf -pvc main.tex && latexmk -c
