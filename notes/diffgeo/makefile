.PHONY: main.pdf lowdim.out

main.pdf: *.tex
	latexmk -pdf -shell-escape main.tex && latexmk -c

tu.pdf: tu.tex
	latexmk -pdf -shell-escape tu.tex && latexmk -c

lowdim.pdf: lowdim.lhs
	pdflatex -shell-escape lowdim.lhs 

lowdim.out: lowdim.lhs
	ghc lowdim.lhs -o lowdim.out



watch: *.tex
	latexmk -pdf -pvc main.tex && latexmk -c
