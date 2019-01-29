PROJECTNAME = assign
OUTNAME = caa-assign6-20161105

PDF = ${PROJECTNAME}.pdf
TEX_MAIN = ${PROJECTNAME}.tex

all: ${PDF}

IMAGES := $(wildcard images/*.jpg images/*.pdf images/*.png)

${PDF}: ${TEX_MAIN} ${IMAGES}
	latexmk -shell-escape -pdf
	mv $(PROJECTNAME).pdf $(OUTNAME).pdf

clean:
	latexmk -C
