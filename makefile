.PHONY: all build serve clean

all: build-website serve
build-builder:
	make -C ./builder/build;
	rm -rf out
	mkdir -p out

run-builder:
	LSAN_OPTIONS=detect_leaks=0 ./builder/build/builder | tee 1-build-log.txt

copy-to-out:
	cp -r prism/* katex/* static/* todo.md resume out/

build-website: build-builder run-builder copy-to-out

serve:
	cd out && python3 -m http.server

upload: build
	ssh root@pixel-druid.com "rm -rf /var/blog/"
	rsync -r --progress -avz ./ root@www.pixel-druid.com:/var/blog
