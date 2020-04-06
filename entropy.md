[9:34 PM] Siddharth Bhat: @Tanmay Kumar Sinha I can do entropy and KL divergence
[9:34 PM] Siddharth Bhat: @Tanmay Kumar Sinha ping when you're around
[9:41 PM] Siddharth Bhat: The TL;DR is this:
0. All my logs will be base 2.
1. We wish to measure how "surprising" something is. We will measure this as Surprise(q) = -log(q) where q is the probability of an event. This way, if an event is deterministic [q=1] then its surprise is 0. If an event is impossible [q=0], then its surprise is infinite. So surprise of an event whose probability is 2^{-n} is going to be n.
2. The entropy of a random variable is the expected surprise we are going to get from it. So:

Entropy(X) =(defn) \sum_{x \in X} Surprise(p(x)) . p(x)
           = E_{x ~ p} (Surprise . p) [ . is function composition ]
[9:41 PM] Siddharth Bhat: So, entropy is "expected surprise"
[9:43 PM] Siddharth Bhat: For example, given a binary random variable, which takes on heads probability q and tails with probability (1 - q). the entropy is going to be:

Entropy(coin_p) = \sum_{x \in {heads, tails}} Surprise(p(x)) . p(x)
                = Surprise(p(heads)) p(heads) + Surprise(p(tails) p(tails)
                = -log(q)q - log(1-q) (1 - q)
[9:44 PM] Siddharth Bhat: We can prove that this is maximum when q = 0.5. This ought to be intuitive, because when q = 0.5, we can't predict anything about the coin toss. If it were slightly higher, then we could guess that it's more likely for it to be heads, so we "know more" about it.
[9:45 PM] Siddharth Bhat: https://en.wikipedia.org/wiki/File:Binary_entropy_plot.svg
File:Binary entropy plot.svg

[9:46 PM] Siddharth Bhat: OK, now we move to the coding theory perspective on all this, which is the one true way (and is, BTW, also the right way to understand K-L divergence IMO)
[9:48 PM] Siddharth Bhat: Note that to define the entropy, all we needed was p(x): X -> [0, 1]. So really, entropy is the property of a probability distribution.
[9:50 PM] Siddharth Bhat: and if we think about it a little carefully [ie, stare at the equation and fiddle with it], we come to the intuition that entropy is high when p(x) is spread out. That is, one can show that the probability distribtution p: X -> R; p(x) = 1/|X| (the uniform distribution) is the unique distribution that has the highest entropy over a set X.
[9:50 PM] Siddharth Bhat: The intuition is that if the distribution is uniform, you can't really guess [this is the same as the coin case], and hence, it must have highest entropy
[9:56 PM] Siddharth Bhat: The coding theory perspective of entropy is this: We have a set X, and we have a probability distribution p(x) on it. We have a sender who is sending elements S = x1, x2, x3, ..., xn with each x drawn from the distribution p(x)  [in a nutshell: sequence S is drawn as an IID sample from X with the distribution p].

Now, how many bits do we need to encode S? We want to mimise the number of bits needed over all possible S.

We can design clever schemes and stupid schemes. Coding theory proves that on average/in expectation, the best encoding scheme will need Entropy(p)  bits to encode each  x_i.   It might spend more bits on some x0 and less bits on some x100, but on average we will need Entropy(p) bits, if the data S is generated using the distribution p.

For an actual example of this, see Huffman coding: it achieves optimal encoding of data, given the probability distributions. It chooses to use a different number of bits for different elements in X. it doesn't clash because it's prefix-free and blah. [https://en.wikipedia.org/wiki/Huffman_coding]
[9:56 PM] Siddharth Bhat: So, the entropy is a measure of a probability distribution that tells us how well we can encode it for communication / how much we can compress information (because we're trying to use the least number of bits possible)
[9:57 PM] Siddharth Bhat: ### K-L divergence
[9:57 PM] Siddharth Bhat: sad, markdown headers don't work
[10:02 PM] Siddharth Bhat: For some intuition about this entropy-as-an-encoding-cost
[10:04 PM] Siddharth Bhat: Q1: assume we go back to the coin C1, and we know that the coin is biased such that it only outputs heads. We're going to be sending the results of the coin flips. How many bits do we need to encode this?

A1: it's zero, because we don't need to transmit anything: the receiver already knows what's going to be sent since the event is deterministic. The sequence of coin flips must be [heads, heads, heads, heads, ...] so there's no point in transmitting it.
[10:05 PM] Siddharth Bhat: This is actually somewhat of a failing of entropy: it only considers the unknown to be information. If you have some complex, deterministic thing, it's not information (in this perspective). Other perspectives such as descriptive complexity (https://en.wikipedia.org/wiki/Kolmogorov_complexity) remedy this.
[10:19 PM] Siddharth Bhat: Q2. we have a coin C2 that takes heads and tails in equal likelihood. How many bits do we need to encode this?

A2: We are forced to encode this using 1 bit, since -log_2(0.5) = 1. This is from Shannon's source coding theorem(https://en.wikipedia.org/wiki/Shannon%27s_source_coding_theorem).
[10:22 PM] Siddharth Bhat: Q3. what is the "good" encoding of a coin that is better than just 0 and 1 in the case, where, say,  heads occurs with probability 0.9 and tails with probability 0.1?

A3. I don't know, I'd like to know. Huffman coding is useless. Entropy tells us that we ought to be able to do this with -0.9 log(0.9) - 0.1 log(0.1) = 0.468 bits. Pinging people who might know:    @Athreya C  ?
[10:25 PM] Siddharth Bhat: OK, now we move on to K-L divergence, now that we've taken a look at entropy-as-surprise and entropy-as-bits-we-have-to-pay
[10:26 PM] Siddharth Bhat: K-L divergence is a divergence (not a distance) between two probability distribution p, q: X -> [0, 1] which tells us how many extra bits  we pay on average if the data came from distribution p but we encoded it as if it came from distribution q.
[10:28 PM] Siddharth Bhat: That is:
- I was given S = [x0, x1, ... xn], which was sampled from X^n ~ p^n. 
- S is handed to an encoder Enc_q which _assumes the data S was sampled from X^n ~ q^n (notice, q not p).
- Enc_q(S) uses more bits than Enc_p(S), since it will have inefficiencies. How much more inefficient? That's the K-L divergence between p and q.
[10:32 PM] Athreya C:
K-L divergence is a divergence (not a distance) between two probability distribution p, q: X -> [0, 1] which tells us how many extra bits  we pay on average if the data came from distribution p but we encoded it as if it came from distribution q.
@Siddharth Bhat This is all GV had to say smh
[10:32 PM] Siddharth Bhat: lmao
[10:32 PM] Siddharth Bhat: wait, I'm writing more
[10:38 PM] Siddharth Bhat: so the formula is defined as:

DKL(P||Q) =
  =\sum_{x \in X} (Surprise(q(x)) - Surprise(p(x))) * p(x)
  =\sum_{x \in X} (-log(q(x)) + log(p(x))) * p(x)    
  =\sum_{x \in X} p(x) * [log(p(x)) - log(q(x))]


I don't have much to offer just from this formula. I take it as a given. I know weird explanations in terms of information geometry, but we won't do that right now.
[10:39 PM] Siddharth Bhat: so let's experiment with it to gain an intuition. 
1. We notice that if p(x) = q(x), it's going to KL(P||Q) is 0
2. It's unclear if it can be negative
3. It's unclear if it's symmetric
[10:40 PM] Siddharth Bhat: I'll show a striking example that will tel us that it is indeed not symmetric, and will help with remembering if the formula is (1) or (2) in the future:

1. p(x) * [log(p(x)) - log(q(x))]
2. p(x) * [log(q(x)) - log(p(x))]
[10:40 PM] Siddharth Bhat: [I'm actually really unsure if anyone will read this at this point. I'm now writing it for my own sake so I can copy-paste and it put it up on my blog :stuck_out_tongue: ]
[10:42 PM] Siddharth Bhat: we go back to the humble unfair coin, for we are gamblers. Assume we have two coins, one which is fair. So this is governed by a distribution called F: {heads, tails} -> [0, 1]. (F for fair). F(heads) = F(tails) = 0.5.

We have another coin which only tosses heads.  This has a distribution H: {heads, tails} -> [0, 1]. H(heads) = 1; H(tails) = 0.
[10:42 PM] Siddharth Bhat: let's compute DKL(F||H) and DKL(H||F).
[10:42 PM] Siddharth Bhat: Before we do that, let's talk intuitions
[10:43 PM] Siddharth Bhat: how do we encode the fair coin? we encode heads as 0 and we encode tails as 1.
[10:43 PM] Siddharth Bhat: how do we encode the heads coin? well, coding theory says "don't". There's nothing to send, so don't bother sending information.
[10:45 PM] Aditya Bharti: Did you stop because you think no one is reading?
[10:46 PM] Siddharth Bhat: no no
[10:46 PM] Siddharth Bhat: I'm deciding how to present this.
[10:49 PM] Aditya Bharti: I've actually seen a derivation of KLD formula based on the "extra bits" concept.
[10:49 PM] Aditya Bharti: Helps to remember the formula (1 vs 2) as well.
[10:49 PM] Aditya Bharti: Please continue for now. I don't have access to a laptop.
[10:49 PM] Siddharth Bhat: - 1A. If I have an encoder Enc_F and I hand it the sequence generated from the heads coin, say HHH, it'll spend 3 bits encoding it as 000.
 
-1B.  If I have an encoder Enc_H and I hand it the sequence HHH, it'll do fine (and not send anything) because there's nothing to be sent, since it assumes anything it has to send will be of the form HHHHH....

So in this case, Enc_F used 3 more bits than Enc_H, assuming the underlying distribution was H.

- 2A.  If I have an encoder Enc_F and I hand it another sequence generated from the fair coin, say HTH, it'll spend 3 bits encoding it as 010.

- 2B. If I have an encoder Enc_H and I hand it HTH it's out of luck. how the fuck is it supposed to send T? It needs to spend infinitely more bits than H to do anything (intuitively)


So in this case, Enc_H used infinitely more bits than Enc_F, assuming the underlying distribution was F.
[10:51 PM] Siddharth Bhat: so, well, clearly, this is not a symmetric concept. If there are things I literally cannot encode, I might spend infinite amount of bits in trying to encode them; If there are things I can encode but am bad at, then you'll spend more bits trying to encode it;
[10:51 PM] Siddharth Bhat: but you'll always need to spend more bits (on average)
[10:51 PM] Siddharth Bhat: So by analyzing the formula, you can check whether (1) or (2) gives you the answer infinity in the case I mentioned above :smile:
[10:51 PM] Siddharth Bhat: and that's how I remember the formula
[10:52 PM] Athreya C:
Interesting choice of an example :3
[10:53 PM] Siddharth Bhat: so, to recap, the K-L divergence of two distributions tells us how "far away" one distribution is at optimally coding data that has been drawn from the other distribution. You can look at it from an adverserial perspective:

[data ~ P(x)] --> [encoder ~ p(x)] (optimal) [baseline]
[data ~ P(x)] --> [encoder ~ q(x)] (adverserial) [how bad are you?]
[10:53 PM] Aditya Bharti: I'm surprised you remember the formula through a slightly contrived example. :stuck_out_tongue:
[10:53 PM] Siddharth Bhat: shrug it's a quick check. if q(x) = 0, then p(x) log(p(x)/q(x)) becomes infinity, so the target distribution is the one in the denominator
[10:53 PM] Siddharth Bhat: aka "out of band stuff costs infinite bits"
[10:54 PM] Aditya Bharti: I like your edits :joy:
[10:54 PM] Siddharth Bhat: argh xD I'm trying to keep the notation consistent xD
[10:54 PM] Siddharth Bhat: (I usually think of it as e(x) and d(x) for encoder and decoder)
[10:54 PM] Athreya C: I mean if you know its positive
[10:55 PM] Athreya C: isn't it easier to check which quantity is positive :stuck_out_tongue:
[10:55 PM] Siddharth Bhat: how?
[10:55 PM] Siddharth Bhat: you can have cancellations
[10:55 PM] Siddharth Bhat: it's a summation
[10:55 PM] Siddharth Bhat: some terms in the sum can be negative, the overall sum is positive
[10:55 PM] Siddharth Bhat: (it's an expectation)
[10:56 PM] Siddharth Bhat: so, how I remember the formula:
[10:58 PM] Siddharth Bhat: 1. it has to be KL(P||Q) = \sum_{x \in X} p(x) * <???> since we're riffing off of entropy
2. it is either log(p(x)/q(x)) or log(q(x)/p(x))
3. it has to be p(x)/q(x) since if q(x) = 0 (ie, q cannot represent something), it is infinitely far away from p, since [log p(x)/0 = log(infty) = infty]
3. it has to be p(x)/q(x) because if p(x) = 0 (ie, the source never gives you this data), I don't care if encode it or not. [following the infotheory convention that [log(0) = 0]
[11:02 PM] Siddharth Bhat: @Aditya Bharti I await your comment; I'm pausing to collect my thoughts
[11:11 PM] Aditya Bharti:
so the formula is defined as:

DKL(P||Q) =
  =\sum_{x \in X} (Surprise(q(x)) - Surprise(p(x))) * p(x)
  =\sum_{x \in X} (-log(q(x)) + log(p(x))) * p(x)    
  =\sum_{x \in X} p(x) * [log(p(x)) - log(q(x))]


I don't have much to offer just from this formula. I take it as a given. I know weird explanations in terms of information geometry, but we won't do that right now.
@Siddharth Bhat so I have some explanation on an admittedly shaky basis.

Let's go back to the "expected number of bits to encode something part". Expected number of bits to encode X ~ p = \sum_{x \in X} p(x)* (-log p(x)). The way I view this formula is:
1. There's a \sum_{x \in X} p(x) because it's a expectation.
2. There's a (-log p(x)) because that's how many bits it takes to encode x coming from distribution X. They way I see it, this is an encoder property, in the sense that the encoder knows the underlying distribution is X, so it uses that distribution to calculate the number of bits.

The probability p(x) is the same in both 1 and 2 because the encoder knows which distribution its inputs are coming from. What happens if the encoder makes the wrong assumption? Say the inputs are coming from distribution p(x) but the encoder assumes q(x). In this case term 1 will be the same (since it's still an expectation and p(x) is the actual distribution of input), but term 2 will equal (-log q(x)), since out theoretical encoder tries to achieve optimality under the assumption of "data coming from q(x).

With this intuition in mind, KLD(from P to Q) = KLD(P||Q) = "how many more [expected] bits under assumption Q, than the actual P?" =  \sum_{x \in X}(bits_Q - bits_P) = \sum_{x \in X} [ p(x) * (-log(q(x)) - p(x) * (-log(p(x)) ] = \sum_{x \in X} [ p(x) * [log(p(x) - log(q(x))] ].

Essentially in my mind, "I know its Q - P, but the formula has a -log so it becomes log p - log q.
[11:12 PM] Aditya Bharti:
@Aditya Bharti I await your comment; I'm pausing to collect my thoughts
@Siddharth Bhat Yeah I was apprehensive to post it. It's already pretty shaky, and any errors would only compound problems.
[11:13 PM] Siddharth Bhat: Go for it, man.
[11:13 PM] Aditya Bharti:
Essentially in my mind, "I know its Q - P, but the formula has a -log so it becomes log p - log q
Does that make sense?
[11:13 PM] Siddharth Bhat: Ah, you just did :smile:
[11:15 PM] Siddharth Bhat: hmm, I see. I'm a little iffy because you can write distributions where log p(x) - log q(x) goes negative for some x = x0, but the overall sum remains positive
[11:15 PM] Siddharth Bhat: which is why I refrain from the "pointwise" perspective
[11:15 PM] Siddharth Bhat: [the pointwise perspective works when a single element is missing as I was saying above, because, well, once you get an infinity, no finite amount of anything can rectify it]
[11:16 PM] Siddharth Bhat: but yeah, I can see how it's a useful mnemonic ^_^
[11:16 PM] Siddharth Bhat: thanks! I suspect it might help me in the future :smile: this is definitely far less roundabout
[11:17 PM] Aditya Bharti:
hmm, I see. I'm a little iffy because you can write distributions where log p(x) - log q(x) goes negative for some x = x0, but the overall sum remains positive
Yup you're right that's why it's a little iffy. In my mind though it just means that, "ok no big deal, just for that particular x, the faulty assumption Q gives less bits than P, but because of more fundamental information theoretic principles, the overall sum must be positive, since P is the true distribution"
[11:18 PM] Aditya Bharti:
thanks! I suspect it might help me in the future :smile: this is definitely far less roundabout
Thank god you agree. I was afraid this plebain logic wouldn't fit with your "one true way" of thinking. :stuck_out_tongue:
[11:20 PM] Siddharth Bhat: xD I mean, even if I disagree, there's nothing "plebian" about it :stuck_out_tongue: If anything, I'm the pleb for rejecting alternative useful perspectives :p
[11:22 PM] Siddharth Bhat: OK, we have not yet proved that K-L divergence is always going to be non-negative
[11:22 PM] Siddharth Bhat: @Aditya Bharti @Athreya C any slick proofs?
[11:23 PM] Siddharth Bhat: so let's do that next
[11:23 PM] Athreya C: Do you have a proof that doesn't rely on concavity/convexity(forgot which)?
[11:26 PM] Aditya Bharti:
@Aditya Bharti @Athreya C any slick proofs?
@Siddharth Bhat I don't remember the formal proofs, only this. Given that we're calculating how inefficient Q is, the total quantity has to be non-negative, since if assumption Q encodes P with less bits than -p log(p), then our assertion that P is the true distribution is false, because information theory (IIRC, maybe some other theorem) tell us -p log(p) is optimal for distribution P.
[11:28 PM] Siddharth Bhat: sure sure :stuck_out_tongue:
[11:29 PM] Siddharth Bhat: OK, I know an extremely slick one (using Bregman divergence), an OK one that requires some work (using Jensen).
[11:29 PM] Siddharth Bhat: I'll do the slick one first
[11:29 PM] Siddharth Bhat: because it's.. nice
[11:29 PM] Athreya C: Jensen in turn relies on Convexity/Concavity though
[11:29 PM] Siddharth Bhat: yeah the slick one does too
[11:42 PM] Siddharth Bhat: OK, back
[11:43 PM] Siddharth Bhat: so, I'm going to define a weird geometric distance that's only valid for convex functions
[11:43 PM] Siddharth Bhat: into which when you plug in our surprisal, life is going to be dandy, and we regenerate the K-L divergence
[11:43 PM] Siddharth Bhat: this gadget is called as the "bregman divergence"
[11:43 PM] Siddharth Bhat: So, the idea is as follows
[11:46 PM] Aditya Bharti: Ok that's got to be the first time I've heard Bregman as a weird gadget :joy:
[11:46 PM] Aditya Bharti: Also one thing before we start @Siddharth Bhat
[11:46 PM] Siddharth Bhat: yes?
[11:47 PM] Siddharth Bhat: wait you've seen this before? xD
[11:48 PM] Aditya Bharti: KLD(P || Q) is said to be "from Q to P", where we still encode P using Q. Idk why the direction of from is reversed
[11:48 PM] Aditya Bharti: Also we plug in "negative surprisal" in Bregman
[11:49 PM] Siddharth Bhat: oh yeah, sure. (for the negative surprisal thing)
[11:49 PM] Aditya Bharti:
wait you've seen this before? xD
@Siddharth Bhat  yeah I had to study this for something else, I forget
[11:50 PM] Siddharth Bhat: it's "from Q to P" because P is the "base" which you're mutating? P is the true distribution?
[11:50 PM] Siddharth Bhat: so you're trying to see how "divergent" you are from Q to P
[11:51 PM] Aditya Bharti: I guess that's the way convention sees it, but it's still something I have to grok
[11:51 PM] Aditya Bharti: Anyway, that ends my side note. Please continue.
[11:53 PM] Siddharth Bhat: So, let us begin with the humble equation of d(x, y) = ||x-y||^2 = <x - y | x - y> where I use < a | b > for the dot-product (a . b) [yay quantum notation
[11:57 PM] Siddharth Bhat: now, we can algebraically manipulate it to get:

lensq(x) = x^2 [assume 1D]
d(x, y) 
  = lensq(x-y)^2 
  = <x - y | x - y> 
  = <x|x> + <y|y> - 2<x|y> [linearity of dot product]
  = <x|x> - <y|y> - 2 <y|x - y> [add and sub 2<y|y>]
  = lensq(x) - lensq(y) - <lensq'(y)|x-y>

since lensq'(y) = d/dy(y^2) = 2y
[11:59 PM] Siddharth Bhat: We can interpret this as:

lensq(x) - (lensq(y) + <lensq'(y)|x-y>)
[11:59 PM] Siddharth Bhat: but 

(lensq(y) + <lensq'(y)|x-y>)


is the equation of a point on a line, starting at y, with slope lensq'(y), moving for a distance of (x - y).
[11:59 PM] Siddharth Bhat: Drawing this out:
[12:15 AM] Siddharth Bhat:

[12:15 AM] Siddharth Bhat: Apologies for the somewhat shitty drawing
[12:15 AM] Siddharth Bhat: the distance we are measuring is f(x) - [f(y) + 2y(x-y)] where f(x) = x^2
[12:15 AM] Siddharth Bhat: So, now, we can choose to generalize this for any convex function f, and it will give us an analogous "distance"
[12:16 AM] Siddharth Bhat: one that measures distance from the value of the function at a point to that of the tangent drawn at another point
[12:16 AM] Siddharth Bhat: this will always be positive (it
[12:16 AM] Siddharth Bhat: (it's clear by just looking at the above figure; for a formal proof, please the convexity of f)
[12:17 AM] Siddharth Bhat: Also note that the same argument will hold in |R^n. We used nothing special about |R here
[12:17 AM] Siddharth Bhat: so we can plug in f((p1, p2, ..., pn)) = \sum_i p[i] log (p[i])  as our convex function f: R^n -> R
[12:18 AM] Siddharth Bhat: and then define the "divergence" D: R^n x R^n -> R; D(x, y) = f(x) - [f(y) + <f'(y)|x-y>]
[12:19 AM] Athreya C: Sorry for interrupting the flow. https://www.youtube.com/watch?v=ZqIlc1ZNbTE (live right now)
A 3 part workshop on MIP* vs RE.
YouTube
Simons Institute
Quantum Protocols: Testing & Quantum PCPs

[12:22 AM] Siddharth Bhat: which on simplification yields:

D(x, y) = f(x) - f(y) - <f'(y)|x-y>
        = f(x) - f(y) - \sum_i (df/dxi)(y) * [x[i] - y[i]]
        = \sum_i x[i] log x[i] - \sum_i y[i] log y[i] -  
          \sum_i [1 + log y[i]] * [x[i] - y[i]]
       = \sum_i x[i] log x[i] + 
         - \sum_i y[i] log y[i] +  \sum_i y[i] log y[i] (= 0)
         (- \sum_i x[i] + \sum_i y[i]) = 0
        - \sum_i x[i] log y[i]
     = \sum_i x[i] (log x[i] - log y[i])
[12:22 AM] Siddharth Bhat: hence, this "divergence" which is K-L divergence derived from the bergemen divergence by using F(x) = sum_i x[i] log x[i]
[12:23 AM] Siddharth Bhat: here is a great visualization link for bergemen divergences: http://mark.reid.name/blog/meet-the-bregman-divergences.html
Meet the Bregman Divergences
An introduction to and survey of some interesting results about Bregman divergences.
[12:23 AM] Siddharth Bhat: anyway, that's my preferred proof for K-L divergence being non-negative
[12:23 AM] Siddharth Bhat: @Athreya C can you give the jensen based one?
[12:24 AM] Siddharth Bhat: or @Aditya Bharti
[12:24 AM] Siddharth Bhat: feel free to ask questions :smile:
[12:29 AM] Tanmay Kumar Sinha: Ι guess the coding theory perspective makes it clear why we use a log in the definition of entropy. But, is there a particular intuition for using log when thinking about it as a measure of 'surprise', apart from the useful property of it being 0 when p is 1( which other functions may satisfy as well)?
[12:30 AM] Siddharth Bhat: if you add the condition that entropy (XY) = entropy(X) + entropy(Y) where X, Y are independent random variables, along with some other intuitive conditions: https://math.stackexchange.com/a/331128/261373
Mathematics Stack Exchange
Intuitive explanation of entropy
I have bumped many times into entropy, but it has never been clear for me why we use this formula:

If $X$ is random variable then its entropy is:

$$H(X) = -\displaystyle\sum_{x} p(x)\log p(x).$$
...

[12:31 AM] Tanmay Kumar Sinha: What does surprise(XY) mean here? Surprise of X and Y?
[12:31 AM] Tanmay Kumar Sinha: As in the event X and Y
[12:35 AM] Siddharth Bhat: you can show that entropy is uniquely characterised by information theoretic considerations
[12:46 AM] Tanmay Kumar Sinha: Nice. Thanks for the explanation.
[12:51 AM] Siddharth Bhat: Hmm, I'm still somewhat dis-satisfied with my K-L divergence explanation
[12:51 AM] Siddharth Bhat: I'll do the fisher information based motivation tomorrow
