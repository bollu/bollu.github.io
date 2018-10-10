all: stat

clean: 
	rm -rf stat bin

stat: stat/program-c.stat stat/program-hs.stat

bin/program-c: program.c
	mkdir -p bin
	clang -O3 program.c -o bin/program-c


bin/program-hs: hs/Main.hs
	mkdir -p bin
	pwd
	cd hs && stack build && cp `stack exec -- whereis program | cut -f 2 -d ' '`  ../bin/program-hs

stat/program-c.stat: bin/program-c
	mkdir -p stat
	sudo perf stat bin/program-c 2>&1 | tee stat/program-c.stat


stat/program-hs.stat: bin/program-hs
	mkdir -p stat
	sudo perf stat bin/program-hs 2>&1 | tee stat/program-hs.stat
