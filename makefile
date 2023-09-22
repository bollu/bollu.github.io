.PHONY: clean serve 

all: build-website serve

clean:
	rm -rf out
	mkdir -p out

build-builder:  builder/
	make -C ./builder/build;

run-builder: README.txt 
	LSAN_OPTIONS=detect_leaks=0 ./builder/build/builder | tee 1-build-log.txt

copy-to-out: prism/ katex/ static/ todo.md README.txt
	cp -r prism katex static prism/* katex/* static/* todo.md resume out/

build-website: build-builder run-builder copy-to-out

serve:
	cd out && python3 -m http.server

upload:  clean build-website
	# ssh-copy-id root@pixel-druid.com # num^3 special^3 letter^3
	ssh root@pixel-druid.com "rm -rf /var/blog/" # num^3 special^3 letter^3
	rsync -r --progress -avz ./ root@pixel-druid.com:/var/blog
	ssh root@pixel-druid.com "chmod -R 777 /var/blog/out/" # num^3 special^3 letter^3
