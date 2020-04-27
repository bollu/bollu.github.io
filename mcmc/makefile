.PHONY: report.pdf plots

report.pdf: report.tex 
	latexmk -pdf -shell-escape report.tex && latexmk -c

plots:
	./plot-euler.py
	./mcmc1d.py
	./hmc1d.py
	./importance-eg.py

	
