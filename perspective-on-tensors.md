https://www.reddit.com/r/math/comments/hqrd1x/how_do_mathematicians_think_about_tensors/fy0ni6r/
I think it's very common for mathematicians to be extremely superficial about this, and you can even see some of that in (most of the responses in) this thread. For instance, insisting on a multilinear understanding, as many do, will inevitably make you consider spinors and spinor fields as overly formal and strange. Insisting on understanding them by their universal property, as some mathematicians will say is "the most truly mathematical way to understand them," is similar but slightly worse. It is similar to the common claim that linear transformations are more natural or "mathematical" than matrices, and that one should try to avoid bases if possible -- maybe we can all agree that the choice of a basis is often "unnatural", but the collection of all bases is a beautiful object ("homogeneous space") which is extremely natural. For me, this object is key to mathematically understanding tensors, as follows. The prerequisite is comfort with the notions of "vector space", "basis", and "group action"

(I think it's interesting that I have never seen mathematicians make the following comment, except in the context of highly formalized presentations of principal bundles and representation theory. Maybe I've read the wrong books.)

  Let V be a n-dimensional real vector space. Let B be the set of bases of V, and let G be the group of invertible n by n real matrices. Then G naturally acts on B by having the matrix [aij] act on [v1,....,vn] to get [a11v1+...+a1nvn,...,an1v1+...+annvn]. You can check that this is a transitive left action.
  Now, given a vector v of V, consider the map fv from B to Rn which sends v to its coordinates relative to the basis. This isn't an arbitrary map; it has the special symmetry that fv(Ab)=ATfv(b) for any basis b and any n by n invertible matrix A. This mapping, from V to the set of mappings from B to Rn which satisfy this symmetry, is a bijection.
  From a high-level view, this can be summed up as saying that "vectors in V can be viewed as certain G-equivariant maps from B to Rn."
  Without the terminology, this is just a sophisticated way to say that one can consider the coordinates of a vector relative to a basis, and that the coordinates of a vector change in a simple way, based on the change-of-basis matrix, when you change the basis.
  To get to tensors, one considers the key phrase "a vector has coordinates" as fundamental and amenable to generalization: to define k-tensors, replace Rn by the vector space of real-valued maps on the (k-times) set product {1,...,n}×...×{1,...,n}. So, for instance, while a vector associates to each basis a list of n numbers, a 2-tensor associates to each basis a list of n2 numbers, although for the sake of understanding the equivariance of a 2-tensor it is not useful to consider it as a list; it is better to consider it as a map from {1,...,n}×{1,...,n} into R; in this case (k=2) one could also consider it as a matrix. Note that one can consider a list of n numbers as a map from {1,...,n} into R.
  This also clarifies the common confusion "is a matrix a tensor"? A 2-tensor is a mapping from the space of bases into the space of matrices. So matrices and tensors are fundamentally different objects; one is a matrix and one is a mapping from a set into the space of matrices. However, the mapping is fully determined by its value on any single input, and so one may use a matrix to define a tensor.

This is the direct mathematical formalization of the physicist's definition, and fully exposes the "tensor transforms like a tensor" comment as an equivariance. There are advantages and disadvantages to working with this definition. It is certainly impossible to fully understand and work with tensors without also understanding the multilinear formulation. But mathematicians should take the physicist's definition seriously, since it has a wider scope and admits important generalizations and extensions which are inaccessible to the multilinear formulation.

Important manifold constructions, such as the Riemann curvature tensor, can also be easily put into this framework.
