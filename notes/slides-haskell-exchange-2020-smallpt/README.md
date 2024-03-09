- https://tex.stackexchange.com/a/372619

TODO: fix Ray, Sphere, UNPACK, Strict to be TEXTTT

9ee060d (HEAD -> master, origin/master, origin/HEAD) Fix difference with C++ version.
d634745 Avoid CPU ieee754 slow paths.
16c5641 Use LLVM backend.
4e141a7 Optimize file writing.
07d68de Custom datatype for 'intersects' parameter passing.
8093155 Hand unroll the fold in intersects.
149bc9f Remove Maybe from intersect(s).
7911c91 Strategic application of strictness.
361eb1e Reduce to only useful strictnesses.
5caf84c Set everything in smallpt to be strict.
eb9a3c1 Remove mutability.
5704ace Convert erand48 to pure Haskell.
66c39a6 Change from maximum on a list to max.
fc36430 Use a pattern synonym to unpack Refl in Sphere.
f3819f8 Mark entries of Ray and Sphere as UNPACK and Strict.
76a82d5 Restrict the export list to 'main'.
0e242c5 Warning cleanup import of smallpt.hs and smallpt.cpp.

0e242c58b59b223289fe10f4bf4a9da36073a6f4
- speedup for |0e242|: 1.00
76a82d538a0f4eb2324539881b4db66229ae7e7b
- speedup for |76a82|: 1.11
f3819f8af83036d551d84f445579292b7e5eee90
- speedup for |f3819|: 1.07
fc364302808708b8a46b9b950fd3a25ff6f0e832
- speedup for |fc364|: 1.07
66c39a61cbcdddc25d7941fdaf0165bdf05586f0
- speedup for |66c39|: 1.08
5704ace89f098c5e7cd846de7707ec0ad11055be
- speedup for |5704a|: 1.10
eb9a3c195fbbe66e97abb77e666a15df0219ba47
- speedup for |eb9a3|: 1.15
5caf84c742134208288641b520bd26eb8e2d97dd
- speedup for |5caf8|: 1.15
361eb1e1202d49a759197fdba005a20e8ab901d5
- speedup for |361eb|: 1.15
7911c91fab7d965cec9456f79ff2896ad95b92b6
- speedup for |7911c|: 1.23
149bc9fa722e18f9e5834f988692731380bfe51a
- speedup for |149bc|: 1.40
809315594cd22c09c921ef37785b5a73af967cd9
- speedup for |80931|: 1.43
07d68dea1c56af87e8c3a0c0370263b364bba567
- speedup for |07d68|: 1.46
4e141a7543a0c6c6326ec4d85c6d06fa1c18ee71
- speedup for |4e141|: 1.46
16c5641e43764247f2b8b30345e0c9b5572fc2eb
- speedup for |16c56|: 2.04
d6347455c57a799970f1f6bc6ed9ac56889e9f12
- speedup for |d6347|: 2.12
9ee060dc6e862dc7c1053b05041008de2f5588b1
- speedup for |9ee06|: 2.32

