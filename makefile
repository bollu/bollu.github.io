.PHONY: all build serve

all: build serve
build:
	make -C ~/blog/builder/build;
	LSAN_OPTIONS=detect_leaks=0 ./builder/build/builder ~/blog/README.md ~/blog/index.html --latex2ascii
serve:
	python3 -m http.server

