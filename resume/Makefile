.PHONY: full-siddharth-bhat.pdf

full-siddharth-bhat.pdf: full-siddharth-bhat.tex
	latexmk -pdf -shell-escape $< && latexmk -c

# Clean auxiliary files
clean:
	latexmk -C

