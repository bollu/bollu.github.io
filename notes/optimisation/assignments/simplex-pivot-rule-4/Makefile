NAME=assign
.PHONY: $(NAME).pdf $(NAME).jpg

$(NAME).pdf : $(NAME).tex
	pdflatex $(NAME).tex
	rifle $(NAME).pdf

$(NAME).jpg: $(NAME).pdf
	convert -density 1000 $(NAME).pdf $(NAME).jpg

