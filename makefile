.PHONY: all build serve clean

all: build serve
build:
	make -C ./builder/build;
	LSAN_OPTIONS=detect_leaks=0 ./builder/build/builder | tee 1-build-log.txt

serve:
	cd out && python3 -m http.server
