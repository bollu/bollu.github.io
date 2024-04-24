# Generate PDF from LilyPond file
%.pdf: %.ly
	lilypond $<

# Rule to generate PDF files for all LilyPond files in the folder
all: $(patsubst %.ly, %.pdf, $(wildcard *.ly))
