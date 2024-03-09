.PHONY:zip
main.pdf: main.tex references.bib
	pdflatex -shell-escape main.tex

zip:
	cd ../ && zip ellipsoid-20161105.zip -r ellipsoid-report
