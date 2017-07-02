+++
date = "2017-06-25 20:09:46+02:00"
title = "alperin groups and representations ex 1 1"
draft = true

+++

- A Group $(G, *, e)$ such that $\forall g \in G, g^2 = e$. Prove that $G$ is abelian.

Consider $(xy)^2 = xyyx = e$, $x^2y^2 = e * e = e$. Hence,
$xyyx = xxyy \implies xy = yx$. $G$ is abelian.



- A group $(G, *, e)$ such that $|G| = mn$, $m, n$ coprime. Show that $\forall g \in G$,
  there exist $a, b \in G$ such that $ab = g = ba$, $a^m = e$, $b^n = e$.

Consider `$|<g>|$`. Since $|<g>|$ divides $G$, $|<g>| = 1, m, n, \text{or} mn$.
(since $m, n$ coprime$.

If $|<g>| = 1$, then $g = e$, pick $a = b = e$. 

If $|<g>| = m$, then pick $a = g$, $b = e$. $a^m = e$ since $|<g = a>| = m$.
Vice versa for $|<g> = n$.

If neither, then $|<g>| = mn$, or $G \equals \frac{Z}/{mnZ}$
