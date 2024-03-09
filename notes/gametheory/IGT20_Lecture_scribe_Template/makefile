.PHONY: zip
main.pdf: main.tex
	pdflatex -shell-escape main.tex

zip:
	zip -r lecture-16.zip main.tex main.pdf igt20.bib makefile
