.PHONY: all 
all: index.html 

index.html: index.md
	ASAN_OPTIONS="detect_leaks=0" builder index.md index.html
