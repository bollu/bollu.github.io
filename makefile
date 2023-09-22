.PHONY: all build serve clean

all: build serve
build:
	make -C ./builder/build;
	rm -rf out
	mkdir -p out
	LSAN_OPTIONS=detect_leaks=0 ./builder/build/builder | tee 1-build-log.txt
	cp -r prism/* katex/* static/* out/

serve:
	cd out && python3 -m http.server

upload: build
	ssh root@pixel-druid.com "rm -rf /var/blog/"
	rsync -r --progress -avz ./ root@www.pixel-druid.com:/var/blog
