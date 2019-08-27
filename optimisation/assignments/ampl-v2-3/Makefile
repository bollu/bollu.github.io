AMPL=/home/bollu/software/amplide.linux64/ampl
.PHONY:all

assign.pdf: solve1 
	pdflatex -shell-escape assign.tex

assign.jpg: assign.pdf
	convert -density 500 -append assign.pdf assign.jpg

solve1: 
	cd q1 && ($(AMPL) solution.run | tee solution.output;  sed "s///g" solution.output -i)


zip:
	zip model.zip -R solution.dat solution.mod solution.run solution.output couenne.opt q1/ q2/ q3/
