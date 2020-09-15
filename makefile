.PHONY: all build serve clean

all: build serve
build: clean
	make -C ~/blog/builder/build;
	LSAN_OPTIONS=detect_leaks=0 ./builder/build/builder ~/blog/README.md ~/blog/ 
serve:
	python3 -m http.server

clean:
	find . -name '*.html' -delete
