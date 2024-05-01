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
	mkdir -p out/static
	cp -r css prism katex static script prism/* katex/* static/* todo.md resume out/
	cp -r sheet-music/*.cropped.svg out/static/

build-website: build-builder run-builder copy-to-out

serve:
	cd out && python3 -m http.server

upload:
	# ssh-copy-id root@pixel-druid.com # num^3 special^3 letter^3
	# make clean-remote
	ssh root@pixel-druid.com "mkdir -p /var/blog/out/"
	rsync -r --progress -avz ./out/ root@pixel-druid.com:/var/blog/out
	ssh root@pixel-druid.com "chmod -R 777 /var/blog/out/"

clean-remote:
	ssh root@pixel-druid.com "rm -rf /var/blog/" # run this only if we want to clean the remote.
