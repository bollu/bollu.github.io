bin/program-c: program.c
	mkdir -p bin
	clang -O3 program.c -o bin/program-c


bin/program-hs: hs/Main.hs
	mkdir -p bin
	ghc -O3 hs/Main.hs -o bin/program-hs

stat/program-c.stat: bin/program-c
	mkdir -p stat
	sudo perf stat bin/program-c > stat/program-c.stat
