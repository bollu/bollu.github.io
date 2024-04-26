# Rule to generate PDF files for all LilyPond files in the folder
all: $(patsubst %.ly, %.pdf, $(wildcard *.ly)) $(patsubst %.ly, %.svg, $(wildcard *.ly)) $(patsubst %.ly, %.midi, $(wildcard *.ly))
# Generate PDF from LilyPond file
%.pdf: %.ly
	lilypond -dcrop $<

%.midi: %.ly
	lilypond --formats=midi $<

%.svg: %.ly
	lilypond -dcrop -dbackend=svg $<
	

