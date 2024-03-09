PROJECTNAME = paper

PDF = ${PROJECTNAME}.pdf
TEX_MAIN = ${PROJECTNAME}.tex

all: ${PDF}

IMAGES := $(wildcard images/*.jpg images/*.pdf images/*.png)

${PDF}: ${TEX_MAIN} ${IMAGES}
	latexmk -shell-escape -pdf

clean:
	latexmk -C
