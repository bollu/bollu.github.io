 Aliens was an easy trick, so didn't really feel like there was enough content. Just a binary search. And aliens cause it popularised the problem, Lagrange multipliers are known since 1800s I guess, but IOI was the big problem with the only solution.
[5:02 PM] Shashwat Goel: idk what lagrange multipliers are so ok
[5:03 PM] Animesh Sinha: They are the just terms you add to model constraints, as in Aliens.
[5:04 PM] Animesh Sinha: But you differentiate and set them to zero, instead of binary searching over them, to get nice results in Physics.
[5:04 PM] Siddharth Bhat: that's a sick explanation @Animesh Sinha  :stuck_out_tongue: they're in fact very geometric
[5:05 PM] Siddharth Bhat: @Animesh Sinha go on xD
[5:05 PM] Siddharth Bhat: I'm interested to know how you think of them
[5:07 PM] Animesh Sinha: So plebian explanation here, treating it as a tool and not a geometric object. Basically, if there are some constraints and a function to minimize under those constraints. Take function + \alpha*constraint, differentiate with the variables you would have, and set it to 0. If this is invariant under alpha, well the constraint you modelled has been satisfied.
[5:07 PM] Animesh Sinha: Alpha, Beta so on for however many constraints.
[5:07 PM] Siddharth Bhat: Why does it work?
[5:08 PM] Animesh Sinha: Cause if the constraint is non-zero, then the derivative will change with change in alpha.
[5:09 PM] Animesh Sinha: Why do you think it works?
[5:09 PM] Siddharth Bhat: I don't understand your reasoning
[5:09 PM] Animesh Sinha: Ah, I saw your saddle point reasoning.
[5:09 PM] Siddharth Bhat: mm? where did you see my reasoning xD
[5:10 PM] Animesh Sinha: I know there will be more, but Wikipedia usually has some lovers of shape and geometry.
[5:11 PM] Siddharth Bhat: this is how I think of it: 
http://bollu.github.io/#the-geometry-of-lagrange-multipliers

[I need to add pictures, will do sometime]
bollu.github.io
bollu.github.io
code + contents of my website, and programming life
[5:11 PM] Siddharth Bhat: [I also need to fix the rendering, to split into multiple pages. Currently MathJax just.. dies trying to render the page]
[5:12 PM] Siddharth Bhat: the TLDR of how I think of it is as follows:
- we have L(x, l) = f(x) + l g(x)
[5:12 PM] Siddharth Bhat: @Animesh Sinha wait, can you explain your reasoning?
[5:13 PM] Siddharth Bhat: @Animesh Sinha because I think you already understood mine?
[5:13 PM] Siddharth Bhat: I want to learn a new way to think about these :smile:
[5:14 PM] Animesh Sinha: Okay, I read, but did not understand fully. My reasoning is the first 3 lines of your reasoning. I never felt that there was a need to go further.
[5:14 PM] Animesh Sinha: You modelled the constraint, and god helps you minimize the function. Reading the levelset thing.
[5:14 PM] Siddharth Bhat: xD
[5:15 PM] Animesh Sinha: Also, the idea that the minima is a Saddle point of the Lagrange function is nice to know.
[5:18 PM] Siddharth Bhat: mm, does the level set idea make sense?
[5:18 PM] Siddharth Bhat: I really wish we could draw pictures in 3D easily
[5:18 PM] Siddharth Bhat: blender is such a bitch to draw math in
[5:19 PM] Animesh Sinha: I didn't get it yet.
[5:20 PM] Siddharth Bhat: OK
[5:20 PM] Siddharth Bhat: so, shall I explain it?
[5:20 PM] Animesh Sinha: Please do.
[5:22 PM] Siddharth Bhat: we know that the direction of the gradient of a function is represented by ∇g. If we move perpendicular to ∇g for some infinitesimal distance, the value of g does not change, yes?
[5:22 PM] Animesh Sinha: Agreed.
[5:23 PM] Animesh Sinha: Wait, what is a levelset, that's where I lost you?
[5:23 PM] Siddharth Bhat: So, given a space V, at some point α, I will decompose the space into V = subspace(∇g(α)) + subspace(∇g(α)_perpendicular)
[5:24 PM] Siddharth Bhat: I'll get to it
[5:24 PM] Siddharth Bhat: does the decomposition make sense?
[5:24 PM] Animesh Sinha: Yep.
[5:24 PM] Siddharth Bhat: ie, we know that ∇g will give us some vector. That the subspace spanned by that vector
[5:24 PM] Siddharth Bhat: and then consider the subspace "perpendicular" to that subspace
[5:24 PM] Siddharth Bhat: so we decompose our original space into these two parts
[5:25 PM] Siddharth Bhat: the part that is "perpendicular" to the gradient subspace what I call the level set. In general, a level set of a function f: V -> W is the set of points that have the same value:

level(w_0) = { v in V : f(v) = w_0 }
[5:26 PM] Siddharth Bhat: so we're interested in:

levelset(g, x_0) = { v in V: g(x) = g(x_0) } 
[5:26 PM] Siddharth Bhat: because we want to keep g(x) = g(x_0) = 0
[5:26 PM] Siddharth Bhat: [we want to keep the constraint satisfied]
[5:26 PM] Siddharth Bhat: while improving on f
[5:27 PM] Siddharth Bhat: we can show that if we are at a point x_0, on moving from x_0 to x_0 + e:
- if we want x_0 + e \in levelset(g, x_0) [g(x_0 + e) = g(x_0)]
-  then (x_0 + e) ⊥ ∇g(x_0)
[5:28 PM] Animesh Sinha: Okay, so each level set has it's own value of g(x), we can think of it as potential energy surfaces, deleting all the other surfaces, taking only g(x) = 0, we should be able to reach the minima of this surface.
[5:28 PM] Siddharth Bhat: yeah
[5:29 PM] Siddharth Bhat: and if we are at some point on the level set g(x) = 0, we want to see how to wiggle from this point
[5:29 PM] Siddharth Bhat: and how this wiggling interacts with the gradient
[5:29 PM] Siddharth Bhat: so, we argue that:
[5:30 PM] Siddharth Bhat: - if we move along ∇g(x_0), then g(x) != g(x + e), because, well, by definition, we have some gradient.
[5:31 PM] Siddharth Bhat: on the other hand, in our lagrange multipler, we get the constraint that
1. ∇g(x_0) is parallel to ∇f(x_0). 
2.if we want to  improve on f, we need to move along ∇f(x_0)
3. ... which means we will also move along ∇g(x_0)
4. ... which means that g(x_0+e) will change [it will not be equal to g(x_0) = 0]
5. ... which means our constraint is no longer satisfied
[5:32 PM] Siddharth Bhat: so the lagrange multipler constraint of ∇f(x_0) + λ∇g(x_0) = 0 can be read as "to improve f, you must change g)
[5:32 PM] Siddharth Bhat: while the second condition g(x_0) = c can be read as "but you are locally satisfied with g
[5:34 PM] Siddharth Bhat: We have L(x, l) = f(x) + l(g). The derivatives read:
1. g(x) = 0.
2. ∇f(x) + l∇g(x) = 0

so in toto, it reads:
1. g(x_0) = 0: you are currently satisfied with g(x_0)
2. ∇f(x_0) + λ∇g(x_0) = 0: to improve f(x) to f(x+e) by wiggling with e, you must change g(x+e) != g(x).

which implies that:
3. to improve f(x) to f(x+e), you must become dis satisfied with g(x+e) != g(x) => g(x+e) != 0
4. hence, the point x_0 is a local optima
[5:34 PM] Siddharth Bhat: @Animesh Sinha thoughts?
[5:35 PM] Animesh Sinha: Wait, reading and rendering.
[5:35 PM] Siddharth Bhat: OK
[5:36 PM] Animesh Sinha: How did we get 1?
[5:37 PM] Animesh Sinha: L(x) = f(x) + \lambda g(x), right?
[5:37 PM] Siddharth Bhat: @Animesh Sinha that's the second condition from our lagrange multipliers gradient, right?
[5:37 PM] Siddharth Bhat: well,it depends on how you pose the problem
[5:37 PM] Siddharth Bhat: you can also pose it as L(x) = f(x) + lambda (g(x) - c)
[5:38 PM] Siddharth Bhat: or you can define it as g'(x) = g(x) - c, and then write it as L(x) = f(x) + lambda g'(x)
[5:38 PM] Siddharth Bhat: [change c <-> 0 approporiately depending on which formulation you're using. I wrote this assuming L(x) = f(x) + lambda (g(x) - c), sorry for the confusion. ]
[5:38 PM] Siddharth Bhat: do you want me to reset everything back to 0?
[5:38 PM] Siddharth Bhat: actually let me do that
[5:39 PM] Animesh Sinha: No, that's okay.
[5:39 PM] Animesh Sinha: I just don't get 1.
[5:40 PM] Siddharth Bhat: can you spell out what 1. is?
[5:40 PM] Siddharth Bhat: there are multiple 1.s floating around :laughing:
[5:40 PM] Animesh Sinha: Ah, got it. X_0/
[5:40 PM] Siddharth Bhat: mm?
[5:40 PM] Animesh Sinha: ∇g(x_0) is parallel to ∇f(x_0). This one.
[5:40 PM] Siddharth Bhat: can you explain to me what you got?
[5:40 PM] Siddharth Bhat: yes
[5:41 PM] Animesh Sinha: I just missed x_0. Okay got it.
[5:41 PM] Siddharth Bhat: OK
[5:41 PM] Animesh Sinha: I always thought of lambda being a cost function multipler, the cost to break the rules.
[5:41 PM] Siddharth Bhat: yeah, that's the "shadow variable" convex optimisation interpretation
[5:41 PM] Siddharth Bhat: I leant that from Boyd
[5:41 PM] Siddharth Bhat: but I find this very natural as well
[5:41 PM] Animesh Sinha: And that if you set the cost to infinity, and the minima still holds, it's the correct one. Yeah, this is great.
[5:42 PM] Animesh Sinha: Very gradient descenty.
[5:42 PM] Siddharth Bhat: indeed.
[5:42 PM] Siddharth Bhat: I learnt it trying to figure out why the fuck lagrange multipliers works geometrically
[5:43 PM] Siddharth Bhat: I think I should clean up the description on my blog; the level sets don't seem to help the explanation. I can just say "along ∇g",  perp. to ∇g. And add the level set stuff at the bottom, perhaps
[5:43 PM] Siddharth Bhat: I find it natural to think of it in terms of level sets, but I can see how it's just extra baggage
[5:44 PM] Siddharth Bhat: go on guys xD @Animesh Sinha @Aditya Bharti
[5:44 PM] Animesh Sinha: Also, I need to learn blender. Maybe I get better at visualising by having something too see. And you can just add this explanation there.
[5:44 PM] Siddharth Bhat: where is "there"?
[5:44 PM] Siddharth Bhat: oh, you mean the blog?
[5:44 PM] Animesh Sinha: Yep.
[5:45 PM] Aditya Bharti: So yeah I agree, the levelset stuff is probably extra. Either explain it more or remove it, it doesn't add to the quality of the argument.
[5:45 PM] Aditya Bharti: My explanation is quite similar.
[5:46 PM] Aditya Bharti: Let's work in 3D for now, where we have to minimize the cost z = f(x,y). The 3D example will become clear in a moment, but it's essentially the same.
[5:47 PM] Aditya Bharti: Since we want to minimize it, we need to move in the direction of -\grad f(x,y). Moving out of the notation.
[5:47 PM] Aditya Bharti: Imagine the 3D surface of f. To minimize it, we just want to move along it's normal. And we will move along it's normal as long as we're allowed to, by the constraints.
[5:49 PM] Aditya Bharti: Once we arrive at the boundary of the feasible region, can't go further "along the normal", but we can still go "at an angle to the normal", if the boundary of the feasible region is at an angle.
[5:50 PM] Aditya Bharti: So the point at which the "boundary of the feasible region" is tangential to the "surface of f", that's the minima we need.
[5:50 PM] Aditya Bharti: It's essentially the same @Siddharth Bhat 's blog post. I'm surprised he didn't mention the words "manifolds" and "tangent spaces" somewhere. I thought this would be a natural argument for him to make.
[5:51 PM] Aditya Bharti: ( I know the current blog post boils down to the same thing, just thought that would be your first thought at explanation :stuck_out_tongue: )
[5:52 PM] Siddharth Bhat: @Aditya Bharti  mm, I don't think the manifold machinery helps much here. What do you have in mind?
[5:53 PM] Aditya Bharti: Locally, the minima is the point where the tangent space of the "cost function manifold/surface" is the constraint function boundary.
[5:54 PM] Siddharth Bhat: ah, sure
[5:58 PM] Siddharth Bhat: Yeah, you can also explain with too much machinery if that is your taste:
- the manifold is defined by M = g^{-1}(0), which is a manifold by using inverse function theorem and whatnot. [It's non-trivial to show that this thing is actually a manifold]
- We wish to move along ∇f to improve f, where ∇ is now the covariant derivative.
- but recall that the covariant derivative on using the levi-cevita connection is precisely the projection of the "real" derivative to the surface of the manifold.
- hence, ∇f = 0, at the constrained optima, since at ∇f = 0, f has no component of the gradient along g.

but is this really how you want to think about the situation @Aditya Bharti ? :stuck_out_tongue:
[5:59 PM] Aditya Bharti: absolutely, most decidedly not
[6:00 PM] Aditya Bharti: I was just mildly, and happily, surprised that you hadn't though. I found your blog post refreshingly plebian and devoid of higher math.

