NAME=assign
$(NAME).pdf : $(NAME).tex
	pdflatex $(NAME).tex

$(NAME).jpg: $(NAME).pdf
	convert -density 1000 $(NAME).pdf $(NAME).jpg

