.PHONY: clean serve run-builder copy-to-out netlify

netlify:
	netlify deploy --build --prod

build-website: run-builder copy-to-out

clean:
	rm -rf out
	mkdir -p out

build/CMakeCache.txt:
	mkdir -p build
	cd build && cmake ../builder/ -DBLOG_ROOT_FOLDER_TRAILING_SLASH=$(git rev-parse --show-toplevel)/

build/builder: build/CMakeCache.txt
	make -C ./build/builder

run-builder: README.txt build/builder
	LSAN_OPTIONS=detect_leaks=0 ./build/builder | tee 1-build-log.txt

copy-to-out: prism/ katex/ static/ todo.md README.txt
	mkdir -p out/static
	cp -r css abcjs prism katex static script prism/* katex/* static/* todo.md resume out/
	cp -r sheet-music/*.cropped.svg out/static/
