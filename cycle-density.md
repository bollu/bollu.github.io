 Has anyone come across any heuristic/algorithm related to "cycle-density", defined as:
Take the vertices of the cycle. Find the number of edges among them and divide by C(num_vertices, 2)
[10:20 PM] Shashwat Goel: Could be maximum cycle density, sum of cycle densitites over all cycles modulo prime, etc. etc.
pkman630
 just landed.
Last Friday at 12:11 PM
[4:37 PM] Siddharth Bhat: Nice, I haven't seen "you may construct additional pylons" before. That's a cool message
[4:40 PM] Siddharth Bhat: @Shashwat Goel regarding cycle density, doesn't nC2 seem like overkill? Like, you're expecting "fully connected cycles"? What's the setting? My intuition for a reasonable definition of cycle density would have been to measure how far away a graph is from chordal, so I would have taken a cycle and computed (#of cycles induced by vertices in C) / (|C| - 3)
[4:41 PM] Siddharth Bhat: (I chose chordal because it's the only class of graphs I know that have extremely nice cycles, perhaps there are other classes...). I'm interested to understand what your conjecture is :D
[5:49 PM] Shashwat Goel: @Siddharth Bhat Is number of induced cycles easier to calculate? Plus your formula doesn't really give an output b/w 0 and 1 by itself right?
[5:50 PM] Siddharth Bhat: @Shashwat Goel oh yeah, I had a particular model in mind; I'll fix the counting, but, like, what are you trying to measure? :slight_smile:
[6:04 PM] Shashwat Goel: https://www.aclweb.org/anthology/L16-1140.pdf - The paper I'm trying to build on.
[6:05 PM] Shashwat Goel: They use cycle density, so I was quite sure at one point more optimized methods to compute cycle density exist, however when I began looking for literature couldn't find any.
[6:05 PM] Shashwat Goel: It's quite an interesting application
[6:07 PM] Shashwat Goel: And even if there are alternate ways to measure 'density' of a cycle different from the edge ratio to completeness, I found this interesting none the less. How to count fast how many chords a cycle has
[6:10 PM] Shashwat Goel: Also in your formula, why |C| - 3? Why not a different denominator.
[12:55 AM] Siddharth Bhat: Hm, because I was thinking of a chordal graph, where you've "triangulated" every cycle so speak, so you have |C| - 3 cycles, I think
[12:55 AM] Siddharth Bhat: well, "fundamental cycle" / "generator of the cycle space"
[12:57 AM] Siddharth Bhat: anyway, I don't know much any graph theory. I know of chordal indirectly since you can color it in polytime, and that's useful because register allocation 'is the same as' (in theory) graph coloring
[12:59 AM] Shashwat Goel: Wait, then what is |C|
[1:00 AM] Siddharth Bhat: |C| is the number of elements in the cycle?
[1:00 AM] Siddharth Bhat: in a chordal graph, every cycle larger than a 3-cycle has  a smaller cycle inside it
[1:00 AM] Siddharth Bhat: so your graph has essentially "triangulated" all the cycles
[1:01 AM] Siddharth Bhat: like, if you take a square, and ask it to be chordal, it will force you to join the diagonal, thereby giving you 2 "small" cycles
[1:01 AM] Siddharth Bhat: and the large cycle (the square) is the sum of the two cycles in cycle space
[1:02 AM] Siddharth Bhat: (I really hope I'm not fucking up, anyone who actually knows graph theory plz step in)
[1:07 AM] Siddharth Bhat: @Shashwat Goel does that..help?
[2:08 AM] Shashwat Goel: no. of elements being no. of vertices?
[2:09 AM] Shashwat Goel: or no. of triangles
[2:09 AM] Shashwat Goel: ig triangles, it makes sense that way.
[2:13 AM] Shashwat Goel: but then, isnt calculating no. of cycles induced cycles kind of equivalent to calculating number of edges among the cycle vertices
[2:22 AM] Siddharth Bhat: mm, I think of it more as counting fundamental cycles in cycle space
[2:22 AM] Siddharth Bhat: which was why the (n 2) count had me surprised
[12:41 PM] Shashwat Goel: Okay, irrespective of the denominator. Any ideas on counting induced cycles/edges within the cycle nodes
[11:36 PM] Siddharth Bhat: what kind of ideas are you looking for?
[11:36 PM] Siddharth Bhat: I don't think I understand your question well
[11:37 PM] Siddharth Bhat: ah, I see, you want an efficient algorithm
[11:47 PM] Siddharth Bhat: for the thing that  I was interested in counting, I believe you can find a basis for the cycle space by computing a spanning tree. then every edge you can add into the spanning tree provides a base cycle. 

We will represent each base cycle as a vector in Z/2Z^E. Each edge E is given a coefficient 1 or 0. It is 1 if it occurs in the cycle, and 0 if not.

Let us call the basic cycles B_i \in Z/2Z^|E|

Now, from the [cycle space theory](https://en.wikipedia.org/wiki/Cycle_basis), we know that we can write any cycle in the graph as a linear combination (over Z/2Z) of the basic cycles B_i. That is, given a cycle C \in Z/2Z^|E|, we decompose it as  C = \sum_i c_i B_i. You can solve for the c_i \in Z/2Z using gaussian elimination, I guess.  Then, the number of "smaller cycles" inside your large cycle C is 2^{number of nonzero c_i}: any "smaller cycle" inside the cycle C can be made by putting together elements of the cycle basis B_i which contribute to C.

Was this like what you were looking for, or did I totally miss the mark? :slight_smile: 

- A link: https://www.codeproject.com/Articles/1158232/Enumerating-All-Cycles-in-an-Undirected-Graph
Enumerating All Cycles in an Undirected Graph
Finding a fundamental Cycle Set forming a complete basis to enumerate all cycles of a given undirected graph
[4:35 AM] Shashwat Goel: I like the approach/perspective a lot. However, just counting edges between cycle nodes is O(N^2) and that is enough to know the number of induced cycles I believe (2*Num_cross_edges + 1?). I believe the gaussian elimination step will be slower here?
[4:36 AM] Shashwat Goel: In any case, this seems to be a nice way of counting once you iterate over all cycles. However the number of cycles can be quite large. I was wondering if there are faster ways to find the most dense cycle through some heuristic, without having to go through all the cycles.
[5:08 AM] Siddharth Bhat: @Shashwat Goel I'm unconvinced that gaussian elimination will be actually slow in practice --- how large are your graphs?
[5:08 AM] Siddharth Bhat: I mean, it might be fun to find the cheapest algorithm, but people have spend quite a lot of time optimising gaussian elimination. It's even cheaper over 0/1 matrices AFAIK
[10:17 AM] Shashwat Goel: Having something fast inside is necessary because remember that there is an outerloop that is quite heavy which justs selects every cycle for counting. The gaussian elimination step takes something ~O(M^3) whereas as I said a 4 line edge counting takes O(N^2logM). Ofc I don't know if implementations make it work faster in practice. I can try checking that.

In any case, a density formula that allows me to prune the cycles, i.e. not compute for every cycle would help cut down the outer loop which is heavier. If I have a step like gaussian elimination inside, I don't find it intuitive enough to find a heuristic to skip cycles from outside. Perhaps I would find it easier with just #no. of edges, and maybe that's just me being a noob.
[10:20 AM] Shashwat Goel: Also, I'm pretty sure speed matters because it takes a noticeable amount of time on toy graphs with ~150 nodes and 500 edges right now. I have to eventually run it for upto 1e4 nodes and 5e4 edges and I wouldn't want each run to take a day
[10:22 AM] Siddharth Bhat: so correct me if I am wrong, but we want to solve for coeff in Cvec = \sum_i coeff_i Bvec_i , or Cvec = coeff Bmat, where Cvec, coeff are 1xn and Bmat is nxn. So, coeff = Cvec Bmat^-1. We only need to invert Bmat once? after which it's's a matmul. But it's really not a matmul, since CVec contains entries that are {0, 1}, so it's really a gather rows of BVec and XOR the bit-vectors, which should be  N^2 in the worst case? It should be something like N^2/4 because you have vector instructions that let you perform 4-8 of the same operations in the same clock cycle, even more if you have a GPU.
[10:24 AM] Siddharth Bhat: and really, it's bitwise XOR, not even add, which ought to be ~2-4x faster than the usual arithmetic stuff thanks to vector instructions?
[10:29 AM] Siddharth Bhat: what am I missing? xD
[10:31 AM] Shashwat Goel: Perhaps you're right. I can't say because this is the first time I'm seeing the cycle space thing :P
[10:31 AM] Siddharth Bhat: ah :slight_smile:

