# Generate PDF from LilyPond file
%.pdf: %.ly
	lilypond -dcrop $<


%.svg: %.ly
	lilypond -dcrop -dbackend=svg $<
	
# Rule to generate PDF files for all LilyPond files in the folder
all: $(patsubst %.ly, %.pdf, $(wildcard *.ly)) $(patsubst %.ly, %.svg, $(wildcard *.ly))

