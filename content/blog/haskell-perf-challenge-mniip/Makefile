all: stat

clean: 
	rm -rf stat bin

stat: stat/program-c.perf stat/program-hs.perf stat/program-hs.prof

bin/program-c: program.c
	mkdir -p bin
	clang -O3 program.c -o bin/program-c


bin/program-hs: hs/Main.hs
	mkdir -p bin stat
	pwd
	cd hs && stack clean && \
		stack build --ghc-options="-O2 -ddump-stg -ddump-cmm -ddump-llvm -ddump-to-file -fllvm" && cp `stack exec -- whereis program | cut -f 2 -d ' '`  ../bin/program-hs
	cp hs/.stack-work/dist/x86_64-linux/Cabal-2.2.0.1/build/program/program-tmp/Main.dump-cmm stat/
	cp hs/.stack-work/dist/x86_64-linux/Cabal-2.2.0.1/build/program/program-tmp/Main.dump-stg stat/
	cp hs/.stack-work/dist/x86_64-linux/Cabal-2.2.0.1/build/program/program-tmp/Main.dump-llvm stat/

bin/program-hs-profile: hs/Main.hs
	mkdir -p bin
	pwd
	cd hs && stack clean && \
		stack build --profile && cp `stack exec -- whereis program | cut -f 2 -d ' '`  ../bin/program-hs-profile


stat/program-c.perf: bin/program-c
	mkdir -p stat
	sudo perf stat bin/program-c 2>&1 | tee stat/program-c.stat


stat/program-hs.perf: bin/program-hs
	mkdir -p stat
	sudo perf stat bin/program-hs 2>&1 | tee stat/program-hs.stat


stat/program-hs.prof: bin/program-hs-profile
	mkdir -p stat
	bin/program-hs-profile +RTS -p -hc
