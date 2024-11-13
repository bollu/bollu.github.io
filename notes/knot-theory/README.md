 # Knot Theory: Conway Polynomial

 - Build a polynomial for any knot.

 # Knot theory from Math at Andrews University

 - [Video](https://www.youtube.com/watch?v=2pmpE1P1xF0&list=PLOROtRhtegr4c1H1JaWN1f6J_q1HdWZOY&index=12)
 - Think of knots as living in S^3

 ## Seifert Surface
 
 - Take the borromean ring. If we naively build a surface, then we get something that's not orientable.
 - Instead, how do we make a compact orientable surface, so we can use the genus of the surface as an invariant?
 - Key idea: smooth knot crossings over. This gives us two disks, and then add ribbons from one disk to another to represent the knot crossings!
 - This gives us an orientable surface.

 ## Calculating the fundamental group of S^3 minus a unknot
 
 - I learnt of this from the knot book.


 #### How to visualize S^3
 

 #### How to visualize S^3

## Slice and Concordance

- Knot is unknot if it bounds a disk in R^3
- Can also say, bounds a disk in S^3?
- Generalize, say a knot is slice iff it bounds a disk in B^4
- This lets us "deform" the knot in interesting ways.
- The key move is to locally "switch" crossings over, which gives us a saddle in B^4. (visualize the fourth axis as time)
- Define an equivalence relation called **condorance**: Two knots are concordant iff **J # -K** is slice.
- Can view this as having a cylinder in B^4 that moves from one knot to the other in the boundary.
- This makes it clear that a knot is slice ifff it is concordant to the unknot, because we can always cap off the unknot.
- To see that this is reflexive.

## Concordance Group

- Since we know that $ K#-K$ is slice, this means that concordance operation forms a group! (Identity is unknot, connect sum is group operaation).
- What is the order of a knot under concordance?

#### Seifert surface revisited

- Recall: two subspaces S and T are in isotopy iff there is a homeo from one to the other.
- Instead of thinking seifert surface as two disks connected by bands, can isotope it to get a single disk connected by a band,
- Now we collect the band information into a matrix (Seifert matrix).
- Key idea: for each band, we make a circle that passes through it. Label it e.g. `x_1, x_2, x_1+, x_2+`.
- The matrix records the linking number of $x_i$ with the pushoff of $x_j$ ($x_j+$).
- Now make this matrix symmetric with $V + V^T$.
- Now, consider the signature of this matrix, i.e. number of +ve eigenvalues minus number of -ve eigenvalues.
- We want to show that the signature is an invariance of concordance.




 #### (Math at andrew university, knot theory](https://www.youtube.com/watch?v=2pmpE1P1xF0&list=PLOROtRhtegr4c1H1JaWN1f6J_q1HdWZOY&index=12)
 - Cool lectures on fundamental group of knot, relationship to linking number, and slices.
 - Also cool lecture on relationship between knots and braids, and how to convert knots to braids, as well as the structure of the braid group.


 - wildberger, course on knot theory
- https://www.youtube.com/watch?v=EBWP1POPc2A&list=PLOROtRhtegr4c1H1JaWN1f6J_q1HdWZOY
- https://www.youtube.com/watch?v=KYddgeiyLJ8
- https://www.youtube.com/watch?v=Rk2LiHzUpps
- fundamental group of knot: https://www.youtube.com/watch?v=R3Hp44AH71k
