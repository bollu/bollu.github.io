.PHONY: main.pdf lowdim.out

lowdim.pdf: lowdim.lhs
	pdflatex -shell-escape lowdim.lhs 

lowdim.out: lowdim.lhs
	ghc lowdim.lhs -o lowdim.out

main.pdf: *.tex
	latexmk -pdf -shell-escape main.tex && latexmk -c

watch: *.tex
	latexmk -pdf -pvc main.tex && latexmk -c
