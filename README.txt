<div class='masthead'>
<div class='masthead-text'>

<h1> A Universe of Sorts
<img style="width: 1em; display: inline-block; float: right" src="/static/banner.png">
<h3> Siddharth Bhat</h3>
</h1>

- [Get in touch (Anonymously)](https://www.admonymous.co/bollu) / Email:  <a href='mailto:bollu@fastmail.com'> `bollu@fastmail.com` </a> / [Grab a video call](https://calendly.com/bollu)
- [Github](http://github.com/bollu) / [Math.se](https://math.stackexchange.com/users/261373/siddharth-bhat) /  [Resume](resume/main.pdf) / [Link hoard](todo.md) / [Photography](https://nx72119.your-storageshare.de/apps/photos/public/3unZrGZ2EsoVZiAJKeWWxtH1SueN0TR5) / <a type="application/rss+xml" href="feed.rss"> RSS feed </a>
- Motto: **It's useful to finish things.**

</div>

<!-- hero: the photo article marked 'hero: yes' renders here as a card. -->

</div>

# Nuclear Energy Prices 

- Henrik's claim: nuclear is the most expensive way to produce energy.
- UK nuclear: "if it ever happens, it would be the most expesntive giga watt hour"
- Henrik's adj to ideal energy mix: renewables plus gas to smooth out the spikes.
- Would be nice to to have nuclear, but is too expensive.

- Sid's model of the world (no numbers): Nuclear is expensive to build, but cheap to run.
- Furthermore, we've gotten worse at building nuclear. So, the aggregate cost of buuilding K nuclea plants
  is offset by improvements. 
- Sid's ideal mix: nuclear + renewables + storage

# Power of LTL versus Power of CTL

- It's classical that LTL and CTL have different power. Here, I write down the example that shows how.

# Nuno's Wisdom On Building Tools And Algorithms
```meta
status: scratch
created: 2026-09-04
last-edited: 2026-09-04
```

- If you can't run the algorithm by hand against the problem, then you have no hope.


  

# IRIS, As Explained To Me By Johannes

```meta
status: scratch
created: 2026-09-01
last-edited: 2026-09-01
```

## Regular Plain Separation Logic

- Have the separating conjuction `*` and the magic wand operator `-*`.
- We use what's called as the 'update pattern' (?) when trying to reason about thing like linked lists.
- For example, consider the program that returns the pointer to the `n`th element on the linked list.

```py
using LinkedList = ptr[(int, LinkedList)]

def nth(x : ptr, i : int) -> ptr:
  match i:
    case 0: return x
    case succ i': return nth(x.next, i')
```



## Concurrent Separation Logic Without Higher Order Nonsense

### Invariants

- An invariant is a proposition that's drawn as if it's in a box (I assume it's a modality?)
- Firstly an invaraiant is duplicable, and is thus not substructral.
- Secondly, an invariant can be 'opened', as long as can re-prove the invariant after closing it.
- I can't remember where ''atomically?'' was important.

Consider a program that adds 2 and 2 to a variable `x`. 

```py
def progi(_i : thread_id, x : int):
   x += 2
   return x

x = 0; progi(0, x) || progi(0, x);
```

- If we just have invariants, then we can keep an invariant that `x` is even,
  and we can prove that both threads maintain this invariant.
- However, we cannot prove that the value of `x` is `4`. 
- So, we invent some more machinery to allow us to prove that `x` is `4`.

### Resource Algebras

- Resource algebra is a commutative monoid that has a predicate `valid` on it.
- For example, for fractional ownership, we use $\mathbb Q \cap [0, \infty)$,
  where `valid(x)` is true if $x \in [0, 1]$.
  We use $0$ as the unit, and $x \cdot y = x + y$.
- This generalizes heaps, because a heap is a RA with finite maps and a bottom element,
  with `valid(h)` is true if `h` is a finite map, and `h1 * h2 = h1 \cup h2` if `dom(h1) \cap dom(h2) = \emptyset`, and bottom otherwise. The unit is the empty heap.
- More interestingly, we can have an RA that has `frac(a)` or `botha(a)` or `full(a)` or `empty(a)` or `bottom`.
- We say that `frac(a) <> full(a) = both(a)`, and `empty(a) <> x = x`, and anything else is `bottom`.


```
x = 0; progi(0, x) || progi(0, x);
```

- We start with the precondition that `x = p1 + p2` and `...`.

### Atomically 

I can't remmber where this played a role.



# My Two Ajjas

```meta
status: essay
created: 2026-08-29
last-edited: 2026-08-29
blurb: The lives of Udupi-Ajja and Huduco-Ajja, written down before the details fade.
```

## Udupi-Ajja

He was born at around the 1930s; WW2 was going on when he was a kid. Of
course, no records were kept, so we don't really know the details. When he
was 5, his mom and dad divorced, and he went to stay with his mom. She was
the most beautiful woman in town :P, and was great at chess. For two years
they lived in @location("Mangalore"), where she worked as a nurse. He knew
about WW2 as a small child, since he mentioned that it impacted their life.
He learnt Konkani while he lived there. She passed away from TB.

After his mom died, he moved in with his dad. Great grandpa hated him,
because he had deserted him, and Ajja was sent to work in the fields. Ajja
always wanted to study. He was sent (sold?) to @location("Kargal", url: "https://maps.google.com/?q=Kargal,+Karnataka")
(close to @location("Joga", url: "https://maps.google.com/?q=Jog+Falls")),
where he worked at a hotel --- unclear for how many years. At the hotel,
someone told him about @location("Ujire") (Dharmastala), and he ran away to
the gurukul there. He learnt Sanskrit, Vedas, etc. there till 11th grade.
This is known _as fact_, cause the person who taught him Sanskrit used to
come home. Supposedly he always came first in class. Bhide taught him
English at the gurukul, and while he was there, he worked(?) at Bhide's
house. This is known cause their families know each other.

After his SSLC, he came to @location("Udupi") and studied his 12th/13th
grade (intermediate) at @location("MGM College", url: "https://maps.google.com/?q=MGM+College,+Udupi"),
as a science major, grabbing one meal a day at the @location("Krishna temple", url: "https://maps.google.com/?q=Sri+Krishna+Matha,+Udupi").
He and his friend Kedlaya grabbed a job at the LIC right after 12th grade.

After 3-4 years of joining the LIC, he fell very sick. That was when he
realized he wished to get married ;) There was _no one_ for him when he
fell sick. He apparently then went to his dad and said "If you don't get me
married, then I'm going to marry someone regardless", to spur his family
into action? He then learnt that there was someone at @location("Narkata").
He went there, and while he was speaking, grandmom came, knocked the jug of
water on the table, and rushed off in anger. Supposedly this was love at
first sight :P "Her fiery spirit" and all that. Grandpa's dad took dowry
from the wedding and pocketed it; grandpa was against the dowry, and learnt
of this much later.

He passed away in 2003, at around 70.

## Huduco-Ajja

Ajja studied till 4th grade, and was then sent to work in
@location("Kerala"). He was then sent to @location("Ullala", url: "https://maps.google.com/?q=Ullal,+Karnataka"),
which is at Karnataka, close to the Kerala border, where he worked at the
home of an Ayurveda Pundit. He learnt his letters by reading signboards,
and then learnt the vocabulary. He used to write Kannada poetry that was
published in the local newspapers. He started translation when he was
30-plus years old, and won the Sahitya Akademi award for translation from
Kannada to Malayalam (2012) --- he had learnt both Kannada and Malayalam.


# Christian Fuchs On Ray Charles Style 60's Funk And Funky Piano

```meta
status: scratch
created: 2026-08-26
last-edited: 2026-08-28
```

- [Jazz Piano Site Jazz Standards Sheet Music](https://www.thejazzpianosite.com/jazz-piano-lessons/the-basics/jazz-piano-sheet-music/)
- [Video](https://www.youtube.com/watch?v=a5WhMqNkuZ0&list=PLLSuwesi2CZLtB_Kp42orBWaSyzfY814r)
- [Playlist](https://www.youtube.com/playlist?list=PLLSuwesi2CZLtB_Kp42orBWaSyzfY814r)


# Mean, Variance And Everything Else As Geometry

```meta
status: scratch
created: 2026-08-12
last-edited: 2026-08-28
```

- [Video](https://www.youtube.com/watch?v=h4QF-2YiM88)
- Space of vectors, where random varibles are vectors with coefficients given by the realization,
  and the all-ones vector is the RV $1$.
- Inner product given by $\langle X, Y \rangle = E[XY]$.
- This gives $\langle X, 1 \rangle = E[X]$.
- [Roman Paolucci: Ito's Lemmas](https://www.youtube.com/watch?v=TgBzqdN24fo)
- [Roman Paolucci: Ito Integration](https://www.youtube.com/watch?v=dUvZ8m3QpeI)



# How to Learn the Altered Scale

```meta
status: scratch
created: 2026-08-08
last-edited: 2026-08-28
```

- Suppose we want to play the `D-G-C` `ii V I` chord progression.
- For G, we can use notes from the altered scale.
- This can be thought of as playing `Ab melodic minor` scale, starting from `G`.
- Recall that melodic minor = major scale with flattened 3rd.
- Alternatively, think in terms of "right hand chords available".
   The left hand plays the shell,
  given by `B-F` (3rd and 7th). Right hand can play:
- C#/D♭ major (D♭–F–A♭): gives ♭5, ♭7, ♭9
- E♭ major (E♭–G–B♭): gives ♭13, root, ♯9
- A♭ minor (A♭–C♭–E♭): gives ♭9, 3, ♭13
- B♭ minor (B♭–D♭–F): gives ♯9, ♭5, ♭7


#### Higher Alterations (when is 9th, 11th, 13th available)

- We stop at 13th, since we get `1-3-5-7-9-11-13` which is 7 notes, so we've run past everything
  by the time we get to 13th.
- We need to know when the 9th, 11th, and 13th are available.
- 9th is always available.
- 11th only over minor, clashes over major.
- Also consider playing with the 5 (#5, b5)

# How to Interpret Variance and Mean Geometrically

```meta
status: scratch
created: 2026-08-12
last-edited: 2026-08-12
```

- [Random Variables Are Just Right Triangles](https://www.youtube.com/watch?v=h4QF-2YiM88)
- Consider a vector space of random variables.
- Then, the vector $1$ is the constant random variable $1$.
- Expectation is the projection onto the vector $1$.
- Now, the orthogonal component is a zero mean random variable,
  and the variance is the squared length of this orthogonal component!


# Jazzy Blues Improv

```meta
status: scratch
created: 2026-08-08
last-edited: 2026-08-08
```

- Use the locked hands / block chords / barry harris
- freely combine major and minor sounds, with a tonal center around A and C.
- Bend almost all notes, keeping in mind that `Ab -> A`, `C# ->D/C` depending on the situation,
  `Bb -> A`, `Eb -> D/E`, `F# -> F`, and that other runs are possible.
- In the right hand side, use diminsheds! they sound really cool, and you can just play major chords
- `BbM` sounds really cool on the left hand.

# Sid's Paper Writing Guide

```meta
status: scratch
created: 2026-07-03
last-edited: 2026-07-08
```

## Abstract

## Introduction

## Technical Writing

## Conclusion

## Evaluation (Writing)

- Phrase as research questions.
- Each research question should have a Yes/No answer.
- Figure out what data to gather to answer research question.
- Break down how datasets were gathered, what tools are being run.
- Compare, and write an analysis per research question.

## Evaluation (Scripting)

- script everything, have scripts generate `plot.tex`, `plot.pdf` and `plot.jpeg`.
- Write scripts that run tools. The tools that are run should print easily parseable stdout,
  return success/failure with exit code, and report errors into stderr.
  The script captures all the output as a jsonl log.
- The aggregation is performed at plotting time, where the jsonl files are aggregated
  into a a final tabulated data. Use `polars` since it's not slop, and use `matplotlib` since it is slop, but
  supports all kinds of crazy layout that no "nice" API can enable you to do.
- take geomean for aggregating times.
- report speedups and slowdowns as ratios, as ratio of geomean = geomean of ratio.
  Also, it's not confusing as *percentage speedup* which is a crazy concept that no one should use.

# Quant Dev Role Prep

```meta
status: scratch
created: 2026-07-08
last-edited: 2026-08-08
```

- [Performance Analysis and Tuning on Modern CPUs](https://github.com/dendibakh/perf-book/releases/tag/2.0_release)
- [Operating Systems: Three Easy Pieces](https://pages.cs.wisc.edu/~remzi/OSTEP/)
- [c++23 the complete guide](https://www.josuttis.com/)


# Misty, Bar Piano Version by Christian Fuchs

```meta
status: scratch
created: 2026-07-01
last-edited: 2026-07-01
```

- [Video](hhttps://www.youtube.com/watch?v=1PyOO-M5zsA&t=775s)

# The Most Satisfying Chord Progression by Christian Fuchs

```meta
status: scratch
created: 2026-06-29
last-edited: 2026-06-29
```

- [Video](https://www.youtube.com/watch?v=ffKAD9ZFHgo&t=4s)
- RH plays Am blues (A C Eb E G), with drone, with the `A` on top with below the below.
- For some crunch, use `G` as the top note.
- Key idea: play 251-251-251 of the form `Dm7 G7sus4 Cmaj` (251)
- Then, treat `Cmaj` as if it were the `2`, to play `Cmaj - Fmaj - Bm7b5`.
- Finally, treat `Bm7b5` as if it were the `2`, to play `Bm7b5 - E7alt - Am`. (Bm7b5 is half-dim).
- Can play `E7alt` as `E7sus4 -> E7`.


# Boogie Woogie in a Minor Key

```meta
status: scratch
created: 2026-06-29
last-edited: 2026-06-29
```

- [Boogie Woogie in A minor by Henri Herbert](https://www.youtube.com/watch?v=HrWIxP8Pxdg)

# FPSanitizer

```meta
status: scratch
created: 2026-06-03
last-edited: 2026-06-03
```

- [Key Idea](https://cp4space.hatsya.com/2026/05/03/schanuels-conjecture-and-the-semantics-of-fpsan/)


# Reflections on Task Creation

```meta
status: scratch
created: 2026-05-13
last-edited: 2026-05-13
```

#### Big Task, Mini Checkpoints

- It's important to work on something large, to feel cool psychologically.
- However, it's impossible to work on large problems, so it's important to learn how to *chunk* large problems
  into smaller problems.
- The small chunks need to be *fun*, which is completely uncorrelated with *important*.


# Playing Funk Piano

```meta
status: scratch
created: 2026-04-16
last-edited: 2026-04-30
```

- [Groove window piano funk groove](https://www.youtube.com/watch?v=P_eEnt3pbNI&list=PLiTb84jJJ9MxcxlvQlsoRNAiVFS2M3VBc&index=14)
- [Funk Chords: Top 5](https://www.youtube.com/watch?v=b0Zbla4Tsfs)
- [groovewindow](https://www.youtube.com/watch?v=V2DiOkLrKvw)
- [funk piano lesson: straight 16ths](https://www.youtube.com/watch?v=CbdUDa6uhWs)
- The key technique I needed to have is to be able to play all notes with my pinky / 4th finger,
  then using the rest of the notes in my right hand to be chord tones.
- [Cool chord progression that enables linear motion of left hand bassline](https://www.youtube.com/watch?v=-27nHg-uhSE)

#### Notes don't matter

- This was pretty mind blowing, but [go watch the video](https://www.youtube.com/watch?v=rEdtUOGCCnU) by open studio, where he argues
  that it's *groove* that matters.
- Rhythm-A-Ning & Straight, No Chaser recommenations for songs to play.


# Lounge Jazz / Bar Piano Ala Christian Fuchs

```meta
status: scratch
created: 2026-04-27
last-edited: 2026-06-29
```


## Basic Chord Progression

- [Christian Fuchs](https://www.youtube.com/watch?v=Wf2RrrVOCbM)
- [Christian Fuchs, Bar Piano Course](https://www.youtube.com/watch?v=EvBFmC3K8Ug&list=PLLSuwesi2CZLj36d44GBdEbEeUhGDoP7-)

### Basic Chord Progressions And Voicings

- Play A minor blues scale on top of these.
- Cmaj: C (bass); 3 5 7 9 (E G B D)
- Am: A (bass); 7 9 3 5 (G B C E)
- Fmaj: F (bass); 3 5 7 9 (A C E G)
- Cmaj:  C (bass); 7 9 3 5 (B D E G)
- Fmaj: F (bass); 3 5 7 9 (A C E G) 
- Cmaj:  C (bass); 3 5 7 9 (E G B D)
- Bm7b5: B (bass); A B D F (b7, 1, 3, 5) [half diminished chord]
- E7alt: E (bass); Ab C D G (major 3rd, sharp 5, 7, sharp 9) [dom7, like the 5 in blues]. This resolves to minor in 1.
- Am: A (bass); 7 9 3 5 (G B C E)
- Can just alternate E7alt and Am on Cmin scale / blues.

### More Jazz Chords ii V I vi

- Dm7: D (bass); F A C E (3 5 7 9) [play on D dorian]. Climb to have the top note be 9th (accentuated note).
- Gdom7:   G(bass); F A B E  [play on G mixolydian]
- Cma7:   C (bass); E G B D [C ionian]. Always end on the B, not on the C (end on the 7th!)
- A7alt:  A (bass); G C C# F (7, #9, 3, #5) (A Bb C C# Eb F# G): 9th and 5th are altered in both directions. Drone with the A note.
  Play the tension notes
- Dm7: End on this.
- Play major blues scale on top of this. 


### Repretoire (Must)

- [15 bar songs must know](https://www.youtube.com/watch?v=l1ua8QhoCg4&list=PLLSuwesi2CZLj36d44GBdEbEeUhGDoP7-&index=4)
- [A touch of blusey](https://www.youtube.com/watch?v=1CL5X9uPjYc&list=PLLSuwesi2CZLj36d44GBdEbEeUhGDoP7-&index=6)
- Summertime (song in Am)
- Georgia on my mind (Fmaj)
- Tennessee Waltz
- Autumn Leaves
- Misty
- Fly me to the moon
- All the things you are
- Stella by starlight

### Magic Voicing System By Frank Mantooth

- [Link to Open Studio video by Adam](https://www.youtube.com/watch?v=fTHhp_2Q-fc)




# WAL and ARIES

```meta
status: scratch
created: 2026-04-27
last-edited: 2026-04-27
```

- [CMU Databases: BusTub](https://15445.courses.cs.cmu.edu/fall2025/syllabus.html)
- [MVCC](https://en.wikipedia.org/wiki/Multiversion_concurrency_control).
- [Refs and Transactions](https://clojure.org/reference/refs).
- [Linked load store conditional](https://en.wikipedia.org/wiki/Load-link/store-conditional).
- WAL: write ahead log for DBs.
- Key point: KV store has fixed size txn, but RDBMS does not.
- On the other hand, a DB will need to write down both what the txn will do, as well as how to revert this txn.
  So there is ARIES, and a do-undo log.
- Also consider the failure case where you run out of disk space when writing a WAL.
- Aries tutorial: https://yashagw.github.io/blog/db-recovery/
- [Database design and implementation](https://simpledb-java.netlify.app/database-design-and-implementation.pdf)
- [State Machine Replication](https://en.wikipedia.org/wiki/State_machine_replication)

# Proof of Godel Incompleteness from Turing Machines

```meta
status: scratch
created: 2026-04-12
last-edited: 2026-04-12
```

- Suppose theory T is complete and consistent.
- Then T gives us a halting oracle, since T 'knows' whether a turing machine `M` halts or does not halt.
- The idea is that when we are given a TM `M` and an input `x`, we can write down the statment
   "M halts on input x" as an arithmetic statement and then ask T whether this statement is true or false.
- This gives us contradiction, as halting oracle cannot exist via halting problem reduction.


# Jazz: Only Rhythm Matters

```meta
status: scratch
created: 2026-04-12
last-edited: 2026-04-12
```

- Thelonious monk: Rhythm-A-Ning 
- Thelonious monk: Straight, No Chaser

# Lounge Jazz Left Hand

```meta
status: scratch
created: 2026-04-12
last-edited: 2026-04-12
```

- [Video](https://www.youtube.com/watch?v=-rFaso7RxDY)
- Swell principle: Give it a breathing feeling. 
  Alternate 10th and 7th when playing II V I. So play 1-10, 1-7, and so o.
- [Block chords: play 10ths](https://www.youtube.com/watch?v=Debp6I51kkI),
  note that's 10th away on the LH.


# Half-Whole Tone Scale As Interlaced Diminished Chords.

```meta
status: scratch
created: 2026-03-24
last-edited: 2026-03-24
```

- Take two diminished chords: `C Eb F# A`, and `Db E G Bb`.
- Interlacing these, we get `C Db Eb E F# G A Bb`, which is the half-tone whole tone scale.
- So, if you know your diminshed chords (which you do, to play joplin and evans),
  then you can easily get the half-whole tone scale, which is super useful for playing over dominant chords.

# Randomized SharpSAT

```meta
status: scratch
created: 2026-03-20
last-edited: 2026-08-08
```

- First idea: take random assignments, and use this to estimate total number of models.
- Next idea: take deterministic partial assignments, then extend these partial assignments randomly. This will use the randomness on a 'smaller domain',
  and exploits linearity of expectation.
- Next refinement: A partial assignment is just a predicate $A$ that we adjoin to our formula.
  Therefore, can we randomly pick such an $A$, such that if we can estimate $|M \models \phi \land A|$, we can estimate $|M \models \phi|$?
  Yes, pick symmetric $A$ that cut down the number of models by a factor of 2 in expectation. For example, pick $A$ to be something like $x_i = x_j$.
- More generally, can pick $x_i = x_j$ with probability 1/2, and $x_i = \neg x_j$ with probability 1/2, and this will give us the same guarantee.
- This can be written as $x_i \oplus x_j = b$, where $b$ is a random bit.
- So, pick $\{ A_k \}$ of the form $x_i \oplus x_j = b$, and then estimate $|M \models \phi \land_k A_k|$ for the full set $A_k$ by random sampling.
  This gives us an estimate of $|M \models \phi|$ as $2^k * |M \models \phi \land A_k|$!
- Why does this work so well in practice? I don't know! I should read Kuldeep Meel's paper to find out.

# Learning All 7th Inversions

```meta
status: scratch
created: 2026-03-04
last-edited: 2026-03-04
```

- Pick D major. Recall, Dmajor7: `D F# A C#`, Ddom7: `D F# A C`, Dm7: `D F A C`.
- play Dmajor(DΔ), Ddom7(1st inversion), Dm7(2nd inversion), Dmajor7(3rd inversion),
  Dmin7(1st inversion).

# Jazz Piano Block Chords Melody Playing

```meta
status: scratch
created: 2026-03-02
last-edited: 2026-04-27
```

- [How to practice and play block chords](https://www.youtube.com/watch?v=AE2tZsGNFvo)
- This is the style I like, it sound 'crunchy' due to the blockiness of the chords.
- [Intro to Block Chords](https://www.youtube.com/watch?v=7dohizUym0M),
- [Minor Block Chords / Barris Harris Method](https://www.youtube.com/watch?v=FGGrHIljeAc).
  Alternate between inversions of `C Em G A` and `D F A C`.
  Play in locked hands style, hands always touching. Melody note on the outside.
- See that we can think of the block chords as being drawn from the bebop scale!
  For example, `C E G A` comes from `C (D) E (F) G (Ab) A`!
  So, this also avoids the 'dissonance' of 'C E G B'.
- [Play dimished chords](https://www.youtube.com/watch?v=Ge6HqCvzscw) and play in contrary motion
  on the right hand side.


# A Different Derivation of the Bepop Notes

```meta
status: scratch
created: 2026-02-05
last-edited: 2026-02-05
```

### Major C bebop

- In the key of C, first play the major 6th. This gives us `C E G A`.
- Now, play the notes that are in the scale but were not played, which gives us `D F B`.
- To make the semitone steps even, we add a `Ab`, which gives `D (Eb E) F (F# G) Ab (A Bb) B`.
- This gives us the bepop scale, which takes all the notes `C E G A` and adds the `D F Ab B` to it.

### Harmonic Minor C bebop

- I don't know if this will work.
- The full harmonic minor scale is `C D Eb F G Ab B`.
- The sixth chord is `C Eb G Ab`.
- The notes in the scale that are not in the chord are `D F B`.
- To make the semitone steps even, we add `E` and `Bb`, which gives us `D (Eb E) F (F# G) Ab (A Bb) B`.
- Doesn't really change much?


### Melodic Minor C bebop

- The full melodic minor scale is `C D Eb F G Ab Bb`.
- The sixth chord is `C Eb G Ab`.
- The notes in the scale that are not in the chord are `D F Bb`.
- No way to make the semitone steps even :(

# Playing over a ii V I with a 3rd Scale.

```meta
status: scratch
created: 2026-02-05
last-edited: 2026-02-05
```

- For a Dm Gdom7 Cm7, play
- Fmaj7 over Dm7
- Bdim7 over G7
- Em7 over Cm7


# Flipped Enclosure Piano Voicings

```meta
status: scratch
created: 2026-02-05
last-edited: 2026-03-24
```

- To play a note, play an enclosure that is diatonic above and chromatic below.
  So in GM, to play G, play `A F# G`.
- Moreover, when playing a sequence, if the sequence goes down, then play the enclosure in an increasing way,
  and vice versa. This creates a jumping sound.


# Jazz Piano Fundamentals Book

```meta
status: scratch
created: 2026-03-24
last-edited: 2026-03-24
```

- [Website](https://jeremysiskind.com/jazz-piano-fundamentals-main-page/)

##### altered dominants 

- [altered dominants 1: sharp 9 use for 'feeling good' funk](https://jeremysiskind.com/jazz-piano-fundamentals-unit-ten/)
- [altered domnants 2 + tritone substitution](https://jeremysiskind.com/jazz-piano-fundamentals-unit-eleven/)
- [improvising over altered dominants](https://jeremysiskind.com/jazz-piano-fundamentals-unit-twelve/)

##### Altered scale

##### minor 2 5 1 and blues scales

- unit 7: BLUES!
- [minor ii V i scale](https://jeremysiskind.com/jazzfundamentals2/unit3/): played over *harmonic* minor, pick every alternate note.
- [improvising over minor ii V i](https://jeremysiskind.com/jazzfundamentals2/unit4/). Use blues scale.
- Use harmonic minor for ii and V, and use melodic minor for i chord (recall in jazz, melodic minor is only for ascending, i.e. major + lowered 3rd).
- [Modal blues](https://jeremysiskind.com/jpf3/unit10home/): phrygian plus minor pentatonic.


#### Bepop & improvising with closed position voicings

- [Book 2](https://jeremysiskind.com/jazzfundamentals2/)
- [Book 2, Unit 8 - Closed Position Voicings Basics, exercise for bepop scale chords that alternates between major 6th and dim7 by Jeremy Siskind](https://www.youtube.com/watch?v=1BU16_t74BA&t=635s)
  Alternate between tonic 6th and diminished seventh chords. use closed position voicings.
  How to harmonize  bebop scale. Play inversions of major 6th and diminished 7th chords.
- [Book 2, Unit 8, closed poisition voicing basics](https://www.youtube.com/watch?v=1BU16_t74BA)
- [Book 2, Unit 9: More Closed Position Voicings](https://www.youtube.com/watch?v=Uxby2w-M4pM)
- [Jazz Fundamentals, volume 2, unit 10: improvising with closed position voicings](https://www.youtube.com/watch?v=d8vzKoANiGk)
- [Jazz Fundamentals Volume 2, Unit 10: drop 2 voicings](https://www.youtube.com/watch?v=dQxD0vvRVm8)
- [Jazz Fundamentals Volume 2, Unit 10: Scale Patterns for Bepop](left hand shuttle)
- drop 2 voicings for "hardly" (https://jeremysiskind.com/jazzfundamentals2/unit10/)
- Use these to play nujabes (reflection eternal, feather, lady brown)

#### ballads

- example ballad: misty
- ballad basics (https://jeremysiskind.com/jazzfundamentals2/unit10/)

#### solo piano devices: left hand shuttle

- [Book 2, Unit 11: Left hand shuttle](https://jeremysiskind.com/jazzfundamentals2/unit11/)
- kenny barron pattern
- peace piece vamp chords


# Stuff I Learnt in 2025 

```meta
status: scratch
created: 2026-01-03
last-edited: 2026-01-03
```

#### Decision Procedures

- Learnt a lot about the Bitwuzla architecture by porting its algorithms to Lean.
- Developed a decision procedure for parametric single width and multi-width bitvectors,
  read most of the literature on bitvector decision procedures in the process.
- Learnt about K-induction, IC3, and Spacer "for real", though I haven't implemented them.
  IC3 in particular is super duper neat.
- Really understood floating point, as we've started porting symfpu to lean.

#### Photography & Travel

- I've kept practising phtography, and being an academic is great for visiting strange new places to take pictures of!
- I got to photograph one of my closest friends' wedding, which was a lot of fun (and a bit stressful!)
- I'm now pretty competent at using [darktable](https://www.darktable.org/).
  I'm hoping to get to contribute to darktable sometime in 2026, but we'll see how that goes!

#### Grant Writing

- I spent more than an epsilon amount of time writing grants in 2025, and I've gotten a lot better at it,
  both at what is expected, as well as the actual writing.
- Takeaway 1: Always delegate your work packages such that even on the day of submission,
  it is defensible that you've "done the work" (at least to prototype stage).
- Takeaway 2: Schedule padding time for projects, and word them such that
  the vision is clear, while leaving room for people to adjust for interests in life based on circumstances.
- Typically, most of the time seems to go into the careful description of the high level idea,
  which make or break the proposal. The actual WPs themselves, while important,
  are much less critical to the success of the proposal.

#### Fiction Reading

- Subscribed to Clarkesworld, so I read a bunch of great science fiction.
- Totally loved 
- *Love in the time of Cholera*
- *Pale Fire*

#### Italian

- Been learning Italian, as Luisa's family don't speak english, and it's her native language.
- This is the most brutal thing I've done in a fair while,
  since it's not a 3 month project, but a years-long project.
- Conjugations are a pain, but vocab is surprisingly "easy", as long as you're not ashamed to repeatedly
  as for words that you think you're supposed to know.

#### Piano

- Learnt a bunch of ragtime, though I still can't play swipsey cakewalk all that well.
- Started mixing in modes, I typically switch between dorian and the harmonic minor scales, as well as the major and minor blues.


#### Goals for 2026

- Write Italian A2 Exam.
- Get a Global Talent Visa.
- Learn some dance form properly (tap? polka?).
- [Learn Jazz Piano](https://www.youtube.com/watch?v=RiCQYWPEhjc&t=941s), and
  relatedly, learn playing good basslines on the piano. [Walk that bass](https://www.youtube.com/watch?v=O2OiircgoRg&t=625s) is a fantastic resource. [How to practice bass lines](https://www.youtube.com/watch?v=KaShTQ-OHqM).


# Modular Arithmetic Decision Procedure

```meta
status: scratch
created: 2025-12-15
last-edited: 2025-12-15
```


- [Modular Arithmetic Decision Procedure by Domagoj and Madanlal](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/tr-2005-114.pdf)
- For bounded integers, actually just perform the hensel lifting. Can handle polynomial fragment. 
- I am not sure why this is supposed to be better than just performing a bit-encoding followed by bitblasting,
  by deriving the width bounds.
- I suppose the idea is that if the system is linear, then linear algebra can help us find the seed solution,
  and lifts should also be faster?

# Nobody's Fault but Mine Piano Chord Voicings

```meta
status: scratch
created: 2025-12-10
last-edited: 2025-12-10
```


- In the treble clef, bar 2, for piano, see the following 

```
A   |    |G(5)|    |G(5)|                               
G(5)|    |    |    |    |    |
F   |    |    |    |    |    | 
E(3)|    |E(3)|    |    |    |
D(3)|    |    |    |D(3)|    |
C(2)|    |    |    |    |C(2)|
B   |    |    |    |    |    |   
A(1)|A(1)|    |A(1)|    |    |
```

- Here, we pick a chord `A-C-E-G`, pick a sus2 note `D`, and then fill in the gaps with `G` and `A` again.


- Similarly, on bar 34, we have, in the treble clef of the piano:

```
G(5)|     |G(5)|     |    |
F(4)|     |    |     |F(4)|
E(3)|     |E(3)|     |    |E(5)    
D   |     |    |     |    |
C(2)|     |    |     |    |
B   |     |    |     |C(2)|
A(1)| A(1)|    | A(1)|    |
```

- Conceptually, we play `1-35-24-5`.
- One way to think about this is that we play `135`, and then we fill in the gaps `24`.
- We round the fill-in-the-gaos with a `1-*-3` pattern.


# Farkas Lemma

```meta
status: scratch
created: 2025-12-09
last-edited: 2025-12-10
```

- [Reference: Advanced Algorithms by Michael X Goemans](https://dspace.mit.edu/bitstream/handle/1721.1/49420/6-854JFall2001/NR/rdonlyres/Electrical-Engineering-and-Computer-Science/6-854JFall2001/FD15AC29-4D14-4733-832C-5626A8B1366E/0/lect9_19.pdf)

#### Statement

- Either $Ax = b$, $x \geq 0$ has a solution, or
- There exists $y$ such that $A^T y \geq 0$, and $y^T b < 0$. (separating hyperplane).

#### Why both cannot occur at the same time

- See that $y^T Ax = y^T b$, so if $y^T A \geq 0$, and there is a solution vector $x \geq 0$ such that $Ax = b$,
  then $y^T b = y^T (Ax) = (y^T A) x \geq 0$.
- This contradicts $y^T b < 0$.

#### Geometric Proof

##### Geometric Lemma

- Let $K$ be a closed covex nonempty set in $R^n$.
- Let $b \in R^n$, $b \not \in K$.
- Define a projection $p_b$ of $b$ onto $K$ as the point $x \in K$ such that  $||b - x||$ is minimized. 
- Then, for all $z \in K$, $(b - p_b)^T (z - p_b) \leq 0$.
- That is, for any point outside the convex set, the vector pointing to the point outside makes a non-acute angle
  with any vector pointing from the projection to a point in the convex set.

#### Proof of Farkas Lemma

- Assume $Ax = b$, $x \geq 0$ has no solution.
- Let $K = \{ Ax : x \geq 0 \}$ be the "positive cone" generated by the columns of $A$.
- These are, intuitively, all linear combinations of the hyperplanes that define the polytope.
- Note that $b \not in k$ (since $Ax = b$, $x \geq 0$ has no solution).
- Let $p_b$ be the projection of $b$ onto $K$. This has a vector $w$ such that $p = Aw$, for some $w \geq 0$.
- Now, we know by the geometric lemma that $(b - p_b)^T (z - p_b) \leq 0$ for all $z \in K$.
- We can rewrite this as $(b - Aw)^T (Ax - Aw) \leq 0$ for all $x \geq 0$. (substitute $z = Ax$, $x \geq 0$).
- TODO


#### Farkas as Interpolant




# Computing with High Dimensional Vectors

```meta
status: scratch
created: 2025-11-17
last-edited: 2025-11-17
```

- Choose binary vectors.
- Lemma 1 (Johnson Lidenstrauss): any n points in high dimensional space can be embedded into O(log n / eps^2) dimensions
  with (1 +/- eps) distortion of distances.
- Lemma 2: Randomly chosen vectors are approximately orthogonal with high probability.
- multiplication is chosen to be XOR.
- Addition is chosen to be bitwise addition of components, followed by MAJORITY
   (keep components that are 1 in at least half the vectors).
- Addition creates vectors that are similar to input vectors.
- Multiplication creates vectors that are dissimilar to input vectors.
- Multiplication is invertible (XOR), and distributes over addition(?!).
- Permutation (rotation)  is invertible, and distributes over addition (?!) and multiplication (?!).
  (I guess this makes intuitive sense, since it's a reindexing, but it's still at least a bit surprising.)
- mutiplication and permutation preserve similarity.
- This gives us a flexible algebraic system to compute with.
- [Stanford Seminar: computing with high dimensional vectors](https://www.youtube.com/watch?v=zUCoxhExe0o)


# Improvising Two Part Invention

```meta
status: scratch
created: 2025-11-16
last-edited: 2025-11-16
```

- Practice moving with a distance of 3rd and 6th between the two voices.
- First practice 3ds and 6th separately. So practice playing a song together with all thirds.
- Then practice playing a song together with all sixths.
- Next, learn to combine thirds and sixths. So switch between 3rds and 6ths at appropriate places.
- Now, add accidentals when moving with the right hand, before reaching a note.
- Once we can do this with the right hand, also add accidentals to the left hand.
- Try switching between left and and right hand being the "leader".

- [Reference: How to Improvise a Two-Part Invention - YouTube](https://www.youtube.com/watch?v=5kNNVNx9TnY)
- [Graham Buckland: Composer on Composing Renaissance](https://www.youtube.com/watch?v=B-NX8qIiaZE)

# Improvise Polyphony in Four Voices

```meta
status: scratch
created: 2025-11-16
last-edited: 2025-11-16
```

- The key idea is that we want to write four voices as 2+2 voices.
- Now, we are interested in "close imitation", where the second voice comes in shortly after the subject.
- There are two key factors: The pitch interval and the time interval.
- An example working close imitation is with pitch:4th, time:1.
- 
- [How to improvise polyphony in four voices according to Santa María](https://www.youtube.com/watch?v=vpfoiwU4rDI)



# How to Improve Evalauation Metrics

```meta
status: scratch
created: 2025-11-14
last-edited: 2025-11-15
```

- Create a histogram to burn down on.
- When you start, you will probably make it via just printing errors, sorted in say lex order.
- This will be too large. Therefore, you will now collapse these into larger equivalence classes, giving you a rougher
  partition of the error space, with a smaller number of buckets.
- The difficulty is to find the sweet spot between too coarse and too fine in the number of buckets.
- Great, now that you have the histogram, you can now start working on the largest buckets first.


# Durable Execution

```meta
status: scratch
created: 2025-11-05
last-edited: 2025-11-17
```

- I learnt of this concept from [absurd.sql](https://lucumr.pocoo.org/2025/11/3/absurd-workflows/),
  and is precisely the vocabulary that I have been looking for.
- I am considering using SQLite for this, as it's the perfect simple backend for the scale of tasks that I have
  (~60,000 to 100,000 jobs, each taking a minute or two).
- However, SQLite doesn't seem to be able to represent queues as well as postgres does, which is a bit unfortunate.
- [To read: Queues via Postgres](https://web.archive.org/web/20190530143429/https://www.2ndquadrant.com/en/blog/what-is-select-skip-locked-for-in-postgresql-9-5/), and are my notes.

# Multi-Width Bitvectors with Append: Using Fundamental Domains?

```meta
status: scratch
created: 2025-10-18
last-edited: 2025-11-05
```

- In discussing with davean, I realized that a good way to think about multi-width BVs is to have a "base length",
  and to write all widths as multiples of this length.
- For now, let's assume we have a base length `w`. Then if we have `a b : BV w`, we see that `a ++ b` will be `BV 2w`.
- When we write FSMs for `a` and `b`, we can do our width-encoding trick, but crucially, we can build FSMs that actually output `a*` and `b*`.
- So, in some sense, we view the normal BVs as the "fundamental domain", and we allow infinite concatenations of these fundamental domains.
- We can keep track of which fundamental domain we are in, and when we concatenate BV expressions, we only move a constant distance away from the fundamental domain
  (since the stx tree is constant). This should let us grab the bits we need!
- This may need us to encode widths as "unary-with-#-symbol", so width 3 is encoded as `000#11`, where `#` is a separator for the last 1 bit.



# Non Linear Theory of 2-Adics Does Not Mix with Bitwise Operations

```meta
status: technical-result
created: 2025-10-17
last-edited: 2025-10-17
blurb: Bitwise AND on the 2-adics defines the naturals, and Hilbert's 10th makes the theory undecidable.
```

- We know that the first order theory of the 2-adics as a field (so with `+`, `-`, `*`, `/`, 0, 1)
  is decidable.
- This decidability result is classical [Diophantine Problems Over Local Fields: III. Decidable Fields](https://www.jstor.org/stable/1970476).
- More recently, there is even a quantifier elimination result for the 2-adics as a field, via a cylindrical algebraic decomposition: [Quantifier elimination in p-adic fields](https://academic.oup.com/comjnl/article-abstract/36/5/419/392350?redirectedFrom=PDF).
- A natural question is: what if we add bitwise operations such as `AND`, `OR`, `XOR` to the 2-adics, interpreted as infinite streams of bitvectors? Does the theory remain decidable?
- We first show that if we have bitwise `AND` (henceforth, `&&`), then we can define the set of natural numbers `N` inside the 2-adics.
- If `N` is definable, then we can encode [Hilbert's 10th problem](https://en.wikipedia.org/wiki/Hilbert%27s_tenth_problem) in our fragment, as it asks if there exists a natural number solution to a multivariate polynomial equation with integer coefficients.
- This can be written as `∃ x1, x2, ..., xn ∈ N : P(x1, x2, ..., xn) = 0`, where we encode `x ∈ N` using our definition of `N` in the 2-adics.

#### 2-adics with AND is undecidable

- Key idea: a natural number is a number `x` whose 2-adic expansion becomes eventually all zeroes.
- This is the same as having some bitmask `M = ..0001111..1` such that `x & M = x`.
- For every such bitmask `M`, see that `M + 1 = P` is a power of 2.
- We can check if a number is a power of 2 with the bit-fiddling trick: `P & (P - 1) = 0`.
- Thus, we can write `x ∈ N` iff `∃ M, P : (M + 1 = P) ∧ (P & (P - 1) = 0) ∧ (x & M = x)`.
- This makes the natural numbers definable in the existential theory of the 2-adics with `&&`,
  and hence we can encode Hilbert's 10th problem as above, which is undecidable.

#### 2-adics with OR is undecidable

- Recall that `!x = -1 - x`, which means bitwise NOT is definable in the 2-adics.
- **Proof:** See that `x + !x = -1` since `-1` is the all-ones string, and in `!x + x`, at every bit location, either `x[1] = 1` and `!x[1] = 0`,
  or vice versa. This means that `x + !x = x || !x = -1`.
- Now, since `x && y = !(!x || !y)`, we can define `&&` in terms of `||` and `!`.
- Thus, if `||` is definable, then so is `&&`, and 2-adics with AND is undecidable as above.

#### 2-adics with XOR is undecidable

- Recall that `x + y = (x XOR y) + 2 * (x AND y)`.
- **Proof:** This can be seen by thinking about the addition of numbers. We compute the carry bits as `2 (x AND y)`, and
  we compute the bits in the current place as `x XOR y`. We add these two up to get `x + y`.
- This means that `x AND y = ((x + y) - (x XOR y)) / 2`.
- Since the 2-adics are a field, we can divide by 2.
- Alternatively, we can perform division by two by using an existential quantifier.
- So given a predicate `∀ x, P(x/2)`, we can replace this with `∀ x y, 2*y = x → P(y)`.
- Thus, if `XOR` is definable, then so is `AND`, and 2-adics with XOR is undecidable as above.


#### 2-adics with other boolean functions.

- Recall that there are 16 boolean functions on 2 bits.
- We use the [table from wolfram mathworld](https://mathworld.wolfram.com/BooleanFunction.html).
- `F[0](x, y) := 0` is all zeroes, which adds no expressive power.
- `F[1](x, y) := x & y` is AND, which is undecidable as above.
- `F[2](x, y) := x & !y` is undecidable since we can get AND by `x AND y = F[2](x, !y)`.
- `F[3](x, y) := x` is projection, which adds no expressive power.
- `F[4](x, y) := !x & y` is undecidable since we can get AND by `x AND y = F[4](!x, y)`.
- `F[5](x, y) := y` is projection, which adds no expressive power.
- `F[6](x, y) := x XOR y` is undecidable as above.
- `F[7](x, y) := x OR y` is undecidable as above.
- `F[8](x, y) := !(x OR y)` is undecidable since we can get OR by `x OR y = !F[8](x, y)`.
- `F[9](x, y) := !(x XOR y)` is undecidable since we can get XOR by `x XOR y = !F[9](x, y)`.
- `F[10](x, y) := !y` adds no expressive power.
- `F[11](x, y) := x OR !y` is undecidable since we can get OR by `x OR y = F[11](x, !y)`.
- `F[12](x, y) := !x` adds no expressive power.
- `F[13](x, y) := !x OR y` is undecidable since we can get OR by `x OR y = F[13](!x, y)`.
- `F[14](x, y) := !(x & y)` is undecidable since we can get AND by `x AND y = !F[14](x, y)`.
- `F[15](x, y) := 1` is all ones, which adds no expressive power.
- Thus, all boolean functions except projections and constants lead to undecidability when added to the 2-adics.

# Succinct Explanation of the Blossom Algorithm

```meta
status: scratch
created: 2025-10-17
last-edited: 2025-10-17
```


- Thanks to Anton Lorenzen for the explanation!


# Using Diminished Chords

```meta
status: scratch
created: 2025-10-09
last-edited: 2025-10-09
```

- Recall: diminished chord is made up entirely of minor 3rds, so `C Eb Gb A` is a `Cdim` chord.
- Technique 1 (Suspended Diminished) : If we are going to play eg `C` chord for a long time, then throw in a `Cdim` in the middle.
  So, play `C E G`, `C Eb Gb Ab`, `C E G`.
- Technique 2 (Drop in Diminished): For stride piano, before playing the 2 chord, a `Dm` or a `Dm7`,
  use the diminished chord up a half step to drop into the `Dm` chord, which is `Eb Gb A C`.
  So we play `Eb Gb A C`, followed by `D F A (C)`.
- Technique 3 (Lift in Diminished): We come from the other direction.
  Go down a half step from `C` to get `Bdim`, which is `B D F Ab`, before playing `C E G`.
- Technique 4 (Coloring Dominant): Play diminished half step up from the root of the V chord.
  So, if we are playing `G7` (the 5 chord in `C`), play `Abdim`, which is `Ab B D F`.
  This gives us a `G7b9` chord, which is a nice tension chord.
  This gives a nice tension before resolving to the `C` chord.


# Spaced Repetition for Learning Italian

```meta
status: scratch
created: 2025-10-09
last-edited: 2025-10-09
```

- I've had mixed success with Anki with using spaced repetition for learning Italian.

- [This is the best tutorial on writing Anki prompts I've stumbled upon](https://andymatuschak.org/prompts/),
  which explains how to write prompts by using cooking as an example activity.
  It even highlights some mistakes that I was making, such as adding mnemonics in the *questions* (not the answers).

# How To Benchmark

```meta
status: scratch
created: 2025-09-29
last-edited: 2025-09-29
```

- I realized that the branch of statistics I need is 'sampling theory',
  which explains how to estimate parameters from populations by means of polling / sampling,
  exactly what we do with our experiments. The following books seems super pertinent:

- Statistical Rethinking:  Bayesian viewpoint.
- "Sampling: Design and Analysis" by Sharon L. Lohr.
  Is in-depth, and has nice examples. Not particularly rigorous.
- "Sampling Techniques 3rd edition - William G. Cochran": Classic, rigorous, dry.
