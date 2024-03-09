AMPL=/home/bollu/software/amplide.linux64/ampl
.PHONY:all

solve: 
	$(AMPL) solution.run | tee solution.output
	sed "s///g" solution.output -i

assign.pdf: solve
	pdflatex -shell-escape assign.tex

assign.jpg: assign.pdf
	convert -density 500 assign.pdf assign.jpg

zip:
	zip model.zip solution.dat solution.mod solution.run solution.output couenne.opt
