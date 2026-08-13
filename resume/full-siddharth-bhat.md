# Siddharth Bhat

Master record of everything: education, positions, publications, software, talks, awards.
The specialized one-page CVs (`cv-compilers.tex`, `cv-verification.tex`, `cv-ml.tex`) are
selections *from* this file. Anything new goes here first.

## Contact

- Email: <siddharth.bhat@cl.cam.ac.uk>
- Phone: +44 7442 785227
- Web: [pixel-druid.com](https://pixel-druid.com)
- GitHub: [github.com/bollu](https://github.com/bollu)
- LinkedIn: [linkedin.com/in/siddharth-bhat](https://www.linkedin.com/in/siddharth-bhat-388b60104/)
- Google Scholar: [profile](https://scholar.google.com/citations?user=7Irb-OUAAAAJ&hl=en)
- Address: 64 Storey's Way, Churchill College, Cambridge CB3 0DS, United Kingdom

## Summary

PhD researcher (Cambridge) working on formally verified decision procedures and mechanical
theorem proving in Lean, with a focus on compiler verification. Co-developer of Lean's
`bv_decide` bitvector solver (5th at SMT-COMP 2025, `QF_BV`) and author of sound-and-complete,
machine-checked decision procedures for parametric bitvectors. Also a decade of
performance-critical C++ inside production compilers and low-level runtimes: #3 contributor to
Polly (LLVM's polyhedral loop optimizer) and primary author of Lean's LLVM backend, lowering an
IR through instruction selection and code generation down to native machine code. Comfortable
reasoning about the machine underneath the language — cache behaviour, memory models, SIMD,
assembly — and optimizing hot code to that level.

Languages: Lean, C++, Haskell, Rust, Rocq/Coq, Python (PyTorch), CUDA, WebAssembly, C.
Infrastructure: LLVM, MLIR, polyhedral compilation, SMT solvers, GPU code generation.

## Expertise

### Automated reasoning: SMT solving and decision procedures (`QF_BV`)

- Co-developed `bv_decide`, Lean's bitvector decision procedure based on verified bitblasting;
  placed **5th at SMT-COMP 2025** in the `QF_BV` division.
- Published new sound-and-complete, formally verified decision procedures for parametric
  (multi-width, width-independent) bitvector theories.
- **#2 contributor to Lean's bitvector theory**, which underpins its SMT automation.
- Ongoing work on mechanized floating-point bitblasting.

### Formal verification (Lean, Rocq) and compiler semantics (LLVM/MLIR)

- Led `lean-mlir`: formal semantics for MLIR in Lean 4, with verified peephole rewriting
  (ITP 2024, Euro LLVM 2025).
- Top-20 contributor to the Lean theorem prover.
- Coauthor of the *Lean 4 Metaprogramming Book* (chapters on tactics and on metaprogramming for
  embedded DSLs).
- Collaboration with VE-LLVM, a formal semantics of the LLVM toolchain in Rocq/Coq.
- Formal specification of polyhedral programs (`PolyIR`, ETH Zurich).
- 5+ talks at LLVM developer meetings on formal semantics for LLVM and MLIR.

### Compilers and high-performance computing (C++, LLVM, MLIR, Haskell, Lean)

- **#3 contributor to Polly**, LLVM's polyhedral loop optimizer: 121 commits to Polly, 6000 LoC
  to LLVM, Fortran support in the GPU/CUDA backend, unified-memory code generation.
- **Primary author of Lean's LLVM backend**: instruction selection and code generation down to
  native machine code.
- **#2 contributor to Asterius**, a Haskell → WebAssembly compiler. Implemented a Haskell-style
  runtime on top of the WASM runtime; contributions merged into Asterius and eventually into
  GHC proper.
- Google Summer of Code mentor for LLVM (2016, 2018).
- Papers on designing MLIR-based IRs for quantum and functional compilation (CC 2021, CGO 2022).
- Stencil and loop-nest optimization in PolyMage, PLUTO, ISL (SC 2017).
- Low-level performance work: rewrote a raytracer in Haskell to match hand-tuned C++
  (Haskell Exchange 2020); lock-free atomic synchronization for real-time audio in the PPSSPP
  C++ emulator.

### AI for mathematics (Lean, F\*, PyTorch)

- RL and retrieval-augmented generation for proofs in the F\* proof assistant at Microsoft
  Research ("Towards Neural Synthesis for SMT-Assisted Proof-Oriented Programming", ICSE 2025,
  **best paper**).
- Dataset defects and evaluation failures in Lean theorem-proving benchmarks (ICML 2026).
- Stronger symbolic baselines for AI geometric theorem proving (NeurIPS 2024 MATH-AI).
- Word embedding representations (RepL4NLP 2020).
- Awarded a [Renaissance Philanthropy AI for Maths](https://www.renaissancephilanthropy.org/mathbench-towards-evaluating-natural-language-proofs)
  grant for *Towards Evaluating Natural Language Proofs*; one of 30 groups funded from 280+
  applicants.

## Education

| Degree | Institution | Dates |
| --- | --- | --- |
| PhD, Computer Science | University of Cambridge | 2024 – Summer 2026 (expected; writing dissertation) |
| PhD, Computer Science | University of Edinburgh (*transferred to Cambridge*) | 2022 – 2024 |
| MS, Computer Science | IIIT Hyderabad, India | 2020 – 2021 |
| BTech, Computer Science | IIIT Hyderabad, India | 2015 – 2020 |

PhD focus: compiler optimization, formal methods, AI for mathematics.
Undergraduate coursework included NLP, deep learning, and word embeddings.

## Industry research

- **Sep–Nov 2024 — Amazon Web Services, Automated Reasoning Group, Austin, TX. Research Intern.**
  Tactics for deciding memory (non-)interference in `lnsym`, a Lean-based ARM symbolic
  simulator. Large-scale symbolic execution and performance optimization.
- **Jul–Sep 2023 — Microsoft Research, Redmond, WA. Research Intern.**
  Retrieval-augmented theorem proving for the F\* proof assistant: a RAG + RL pipeline, with
  neural models trained in PyTorch guiding proof search alongside symbolic search.
  Published at ICSE 2025 (best paper).
- **May–Jul 2019 — Tweag.io, Paris, France. Research Intern.**
  Re-implemented portions of the GHC runtime for
  [Asterius](https://github.com/tweag/asterius/commits?author=bollu), a Haskell-to-WebAssembly
  compiler. Haskell, C, WebAssembly.

## Academic research and internships

- **Summer 2018 — ETH Zurich, Switzerland. Research Intern.**
  Formal verification of polyhedral compilation; built
  [PolyIR](http://github.com/bollu/polyir), a formal specification of polyhedral programs.
- **Summer 2018 — Google Summer of Code, Polly Labs. Mentor.**
  Mentored a project enabling Polly's loop optimizations in Chapel.
- **Mar–Dec 2017 — ETH Zurich, Scalable Parallel Computing Lab. Research Intern.**
  GPU code generation in Polly: CUDA backend features for Fortran workloads on
  performance-critical loop nests.
- **May–Jul 2016 — IISc Bangalore, India. Research Intern.**
  PolyMage, a DSL compiler for data-parallel stencil computation. Contributed to ISL and PLUTO;
  implemented tiling patterns and stencil optimizations.
- **Summer 2015 — Google Summer of Code, Google. Student.**
  VisPy, a Python/OpenGL graphics library.
- **Jul 2023 — Adjoint School, Glasgow.**
  Markov categories and their relationship to probabilistic programming.
- **Winter 2018/2019 — Teaching Assistant, IIIT Hyderabad.**
  *Principles of Programming Languages* (lecture notes, assignments, grading) and
  *Natural Language: Applications* (sessions on `word2vec`, `GloVe`, `fasttext`).

## Publications

### First author

1. **Towards Mechanized Floating Point Bitblasting.** *Siddharth Bhat*, Abdalrhman Mohamed,
   Tobias Grosser. POPL 2027 (under review).
2. **Sound and Complete Solving for Multi-Width Parametric Bitvectors via Principled
   Reductions.** *Siddharth Bhat*, Léo Stefanesco, Tobias Grosser. OOPSLA 2026
   (accepted with minor revisions).
3. **Certified Decision Procedures for Width-Independent Bitvector Predicates.**
   *Siddharth Bhat*, Léo Stefanesco, Chris Hughes, Tobias Grosser. OOPSLA 2025.
4. **Verifying Peephole Rewriting in SSA Compiler IRs.** *Siddharth Bhat*, Alex Keizer,
   Chris Hughes, Andres Goens, Tobias Grosser. ITP 2024.
5. **Lambda the Ultimate SSA.** *Siddharth Bhat*, Tobias Grosser. CGO 2022.
6. **Word Embeddings as Tuples of Feature Probabilities.** *Siddharth Bhat*, Alok Debnath,
   Souvik Banerjee, Manish Shrivastava. RepL4NLP 2020.

### Collaborations

1. **Faults in Our Formal Benchmarking: Dataset Defects and Evaluation Failures in Lean Theorem
   Proving.** Pawan Sasanka Ammanamanchi, *Siddharth Bhat*, Stella Biderman. ICML 2026.
2. **Interactive Bit Vector Reasoning using Verified Bitblasting.** Henrik Böving,
   *Siddharth Bhat*, Alex Keizer, Luisa Cicolini, Leon Frenot, Abdalrhman Mohamed,
   Léo Stefanesco, Harun Khan, Josh Clune, Clark Barrett, Tobias Grosser. OOPSLA 2025.
3. **Towards Neural Synthesis for SMT-Assisted Proof-Oriented Programming.** Saikat Chakraborty,
   Gabriel Ebner, *Siddharth Bhat*, Sarah Fakhoury, Sakina Fatima, Shuvendu Lahiri,
   Nikhil Swamy. ICSE 2025 (**best paper**).
4. **Verifying Wu's Method can Boost Symbolic AI to Rival Silver Medalists and AlphaGeometry to
   Outperform Gold Medalists at IMO Geometry.** Shiven Sinha, Ameya Prabhu,
   Ponnurangam Kumaraguru, *Siddharth Bhat*, Matthias Bethge. NeurIPS 2024 Workshop MATH-AI.
5. **Rewriting Optimization Problems into Disciplined Convex Programming Form.**
   Ramon Fernandez Mir, *Siddharth Bhat*, Andres Goens, Tobias Grosser. CICM 2024.
6. **Guided Equality Saturation.** Thomas Koehler, Andres Goens, *Siddharth Bhat*,
   Tobias Grosser, Phil Trinder, Michel Steuwer. POPL 2024.
7. **QSSA: An SSA-based IR for Quantum Computing.** Anurudh Peduri, *Siddharth Bhat*,
   Tobias Grosser. CC 2021.
8. **Optimizing Geometric Multigrid Computation using a DSL Approach.** Vinay Vasista,
   Kumudha KN, *Siddharth Bhat*, Uday Bondhugula. Supercomputing (SC) 2017.

## Software and open source

- **[Lean 4](https://github.com/leanprover/lean4/pulls?q=author%3Abollu)** — co-developed the
  verified bitblasting theory behind `bv_decide`; primary author of Lean's LLVM backend;
  top-20 contributor overall, #2 to the bitvector theory.
- **[Polly (LLVM)](https://polly.llvm.org/)** — #3 contributor to LLVM's polyhedral loop
  optimizer: 121 commits, 6000 LoC, Fortran support and unified-memory CUDA code generation.
  [Commits](https://reviews.llvm.org/p/bollu/).
- **[Asterius / GHC](https://github.com/tweag/asterius/commits?author=bollu)** — #2 contributor
  to this Haskell → WebAssembly compiler; reimplemented the GHC runtime on WASM, later merged
  into GHC proper.
- **[lean-mlir](https://github.com/opencompl/lean-mlir)** — formal semantics for MLIR in Lean 4,
  with verified peephole rewriting (ITP 2024).
- **[Rocq/Coq](https://github.com/coq/coq/issues?&q=author%3Abollu)** — issues, bug fixes,
  developer documentation.
- **[VE-LLVM](https://github.com/vellvm/vellvm/issues?&q=author%3Abollu)** — collaboration on a
  formal semantics of the LLVM toolchain in Rocq/Coq.
- **[PLUTO](http://pluto-compiler.sourceforge.net/) &
  [PolyMage](http://mcl.csa.iisc.ac.in/polymage.html)** — loop-nest optimizers: found bugs in
  diamond tiling and fixed the PLUTO API; stencil and time-iterated-stencil support in PolyMage.
- **[Lean 4 Metaprogramming Book](https://github.com/arthurpaulino/lean4-metaprogramming-book)** —
  authored the chapters on tactics and on metaprogramming for embedded DSLs.
- **[Simplexhc](http://github.com/bollu/simplexhc)** — a compiler for a subset of Haskell
  applying polyhedral-compilation ideas to a lazy functional language, targeting LLVM. 64 stars.
- **[lz](https://github.com/bollu/lz)** — an MLIR-based compiler backend for Lean 4.
- **[symengine.hs](https://github.com/symengine/symengine.hs)** — GSoC 2016; Haskell bindings to
  the SymEngine C++ symbolic manipulation library.
- **[VisPy](https://github.com/vispy/vispy/commits?author=bollu)** — GSoC 2015; rewrote the
  scene graph for performance, added visuals and a high-level plotting API, implemented
  auto-resizing with Cassowary.
- **[PPSSPP](https://github.com/hrydgard/ppsspp/commits?author=bollu)** — C++ PSP emulator:
  wrote most of the touch-handling code, implemented atomic locks for audio performance.
- **Rust** — contributions to the compiler and ecosystem; part of
  [Piston](https://github.com/PistonDevelopers), a Rust game-engine group.
- **Haskell** — reported and fixed bugs in *stack*, *stackage*, *diagrams*, GHC
  ([GHC commits](https://phabricator.haskell.org/p/bollu/)).
- **[Sublime Bookmarks](https://github.com/bollu/sublimebookmark)** — Sublime Text plugin,
  26k downloads.
- **[Cellular Automata](https://www.github.com/bollu/cellularAutomata)** — comonadic cellular
  automata in Haskell. 130 stars.
- **[Teleport](http://bollu.github.io/teleport)** — project switcher in Haskell, published as a
  Literate Haskell tutorial. 90 stars.
- **[TIMi](http://github.com/bollu/timi)** — visual interpreter for the template instantiation
  machine. 51 stars.

## Talks and presentations

- **Euro LLVM Dev 2025** —
  [How to trust your peephole rewrites: automatically verifying them for arbitrary width!](https://www.youtube.com/watch?v=WtsInfbzxjs)
- **US LLVM Dev 2024** —
  [lean-mlir: a workbench for formally verifying peephole optimizations in MLIR](https://www.youtube.com/watch?v=4lh2NnLOxvQ)
- **US LLVM Dev 2023** —
  [(Correctly) extending dominance to MLIR regions](https://www.youtube.com/watch?v=VJORFvHJKWE)
- **US LLVM Dev 2023** —
  [MLIR side effect modelling](https://www.youtube.com/watch?v=6bDKasLZyxU)
- **Euro LLVM Dev 2022** —
  [MLIR for functional programming](https://www.youtube.com/watch?v=cyMQbZ0B84Q)
- **FPIndia 2021** — [Equality saturation](https://www.youtube.com/watch?v=cfdII1jDJYU)
- **Haskell Exchange 2020** —
  [Optimizing `smallpt-hs`, a raytracer ported to Haskell, to beat C++ performance](https://skillsmatter.com/skillscasts/14910-smallpt-hs-porting-a-raytracer-s-performance-to-haskell)
  ([slides](https://github.com/bollu/slides-haskell-exchange-2020-smallpt))
- **Functional Conf 2019** —
  [monad-bayes: probabilistic programming in Haskell](https://confengine.com/conferences/functional-conf-2019/proposals)
  ([slides](https://github.com/bollu/functionalconf-2019-slides-probabilistic-programming))
- **Theory seminar, IIIT-H, Winter 2019** —
  [Impossibility of compass-straightedge constructions via field theory](https://faculty.iiit.ac.in/~theory/seminar/talks/trisecting-ruler-compass/)
- **ETH Zurich** — [The Barvinok algorithm for counting lattice points in polyhedra](https://github.com/bollu/barvinok/blob/master/slides.pdf)

## Awards and grants

- **Renaissance Philanthropy, AI for Maths** grant — *Towards Evaluating Natural Language
  Proofs*; one of 30 research groups funded from 280+ applicants.
- **Best paper, ICSE 2025** — *Towards Neural Synthesis for SMT-Assisted Proof-Oriented
  Programming*.
- **5th place, SMT-COMP 2025**, `QF_BV` division, for `bv_decide`.
- **Google Summer of Code** — student 2015 (VisPy), selected 2016 (SymEngine), mentor 2016 and
  2018 (LLVM/Polly).
- [math.stackexchange](http://math.stackexchange.com/users/261373/siddharth-bhat) — 8312
  reputation, top 4% overall; abstract algebra and differential/algebraic geometry.
