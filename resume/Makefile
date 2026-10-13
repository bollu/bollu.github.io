CVS := cv-full cv-compilers cv-verification cv-ml
PDFS := $(addsuffix .pdf,$(CVS))

.PHONY: all cvs full-siddharth-bhat.pdf clean

# `make` builds the one-page CVs: cv-full (consolidated; the one the website
# links to) and the three specialized tracks (compilers, formal verification,
# machine learning). All are selections from full-siddharth-bhat.md, the master
# record; style lives in cvstyle.sty.
all: cvs

cvs: $(PDFS)

$(PDFS): %.pdf: %.tex cvstyle.sty
	latexmk -pdf -interaction=nonstopmode $< && latexmk -c $<
	@pdfinfo $@ | grep -q '^Pages: *1$$' || echo "WARNING: $@ is not one page"

# The legacy long-form moderncv resume.
full-siddharth-bhat.pdf: full-siddharth-bhat.tex
	latexmk -pdf -shell-escape $< && latexmk -c

clean:
	latexmk -C
