.PHONY: all build serve clean

all: build serve
build: clean
	make -C ./builder/build;
	LSAN_OPTIONS=detect_leaks=0 ./builder/build/builder

serve:
	cd out && python3 -m http.server

clean:
	find . -name '*.html' ! -name 'chat.html'  -delete
