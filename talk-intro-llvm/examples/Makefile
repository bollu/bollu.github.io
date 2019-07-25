.PHONY: ite clean ite-mem2reg loop1d

ite: 
	clang -O0 ite.c -S -emit-llvm -o ite-O0.ll -Xclang -disable-O0-optnone
	clang -O1 ite.c -S -emit-llvm -o ite-O1.ll -Xclang -disable-O0-optnone

ite-mem2reg: ite
	opt -mem2reg ite-O0.ll -S -o ite-mem2reg.ll
	opt -mem2reg ite-O0.ll -o ite-mem2reg.bc

ite-instcombine: ite-mem2reg
	opt -S -instcombine ite-mem2reg.ll -o ite-instcombine.ll

ite-simplifycfg: ite-instcombine
	opt -S -simplifycfg ite-instcombine.ll -o ite-simplifycfg.ll

ite-machineinsts: ite
	llc -print-machineinstrs ite.ll

clean:
	rm *.ll

loop1d: 
	clang -O0 loop1d.c -S -emit-llvm -o loop1d-O0.ll -Xclang -disable-O0-optnone
	clang -O1 loop1d.c -S -emit-llvm -o loop1d-O1.ll -Xclang -disable-O0-optnone

loop1d-instcombine: loop1d
	opt -S -mem2reg -instcombine -simplifycfg loop1d-O0.ll -o loop1d-instcombine.ll

# "Scalar Evolution"
loop1d-scev-analysis: loop1d-instcombine
	opt -analyze -scalar-evolution loop1d-instcombine.ll 
	opt -analyze -loops loop1d-instcombine.ll 

loop1d-indvars: loop1d-instcombine
	opt -S -indvars loop1d-instcombine.ll -o loop1d-indvars.ll

loop1d-dce: loop1d-indvars
	opt -S -loop-deletion loop1d-indvars.ll -o loop1d-dce.ll
