subtle.out: subtle.cpp
	g++ -fsanitize=address -fsanitize=undefined -Wall -Werror subtle.cpp -o subtle.out -static-libasan

slides.pdf: slides.tex
	pdflatex -shell-escape slides.tex
