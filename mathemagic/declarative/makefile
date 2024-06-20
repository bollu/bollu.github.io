.PHONY: all server
all: index.html server

server:
	python3 -m http.server

index.html: index.md anim.out minanim.js script.js
	ASAN_OPTIONS="detect_leaks=0" ./anim.out > ANIMGENERATED.js
	ASAN_OPTIONS="detect_leaks=0" builder index.md index.html

anim.out: anim.cpp
	g++ -g -O1 -std=c++11 -fsanitize=address -fsanitize=undefined anim.cpp -o anim.out

