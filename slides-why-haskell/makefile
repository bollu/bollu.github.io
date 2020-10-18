slides.pdf: slides.tex
	pdflatex -shell-escape slides.tex

subtle.out: subtle.cpp
	g++ -fsanitize=address -fsanitize=undefined -Wall -Werror subtle.cpp -o subtle.out -static-libasan

k.out: k.cpp
	g++ -fsanitize=address -fsanitize=undefined -Wall -Werror k.cpp -o k.out -static-libasan

