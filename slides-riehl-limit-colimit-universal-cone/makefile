.PHONY: show
show: slides.pdf
	okular -unique slides.pdf
slides.pdf: slides.tex
	pdflatex -shell-escape slides.tex
