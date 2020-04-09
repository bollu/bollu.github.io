.PHONY: main.pdf

assign3.pdf: assign3.tex
	latexmk -pdf -shell-escape assign3.tex && latexmk -c

assign2.pdf: assign2.tex
	latexmk -pdf -shell-escape assign2.tex && latexmk -c


assign1.pdf: assign1.tex
	latexmk -pdf -shell-escape assign1.tex && latexmk -c

main.pdf: *.tex
	latexmk -pdf -shell-escape main.tex && latexmk -c

watch: *.tex
	latexmk -pdf -pvc main.tex && latexmk -c
