 the story is this:
- a value is a isomorphic to a continuation acont := forall r. (a -> r) -> r. that is, you tell me how you want to process a, and I'll do the processing for you
- You can write down what the continuation for nat is supposed to be: natcont: forall r. (nat -> r) -> r.
- You can massage the type to arrive at forall r. (r -> r) [how to handle S] -> r [how to handle Z] -> r [output]
[11:29 AM] Siddharth Bhat: for example, the continuation for bool is going to be this:

boolcont := forall r. (bool -> r) -> r
 := forall r. (r, r) -> r


this is because a function A -> B is ismorphic to B^|A|. so, bool -> r is isomorphic to r^|bool| = r^2 = (r, r)
[11:30 AM] Siddharth Bhat: so, the intuition we get from this case is:
- sums turn into products.
[11:31 AM] Siddharth Bhat: we can continue to massage the type further:

boolcont := forall r. (bool -> r) -> r
 := forall r. (r, r) -> r [bool -> r ~= r^|bool| ~= (r, r)]
 := forall r. r -> r -> r [currying: (a, b) -> c ~= a -> b -> c]
[11:31 AM] Siddharth Bhat: so we can encapsulate the rule as:
- for every "choice" in a sum type, take a new parameter in the "continuation type"
[11:31 AM] Siddharth Bhat: @Aditya Bharti tell me if this is making sense at all
[11:31 AM] Siddharth Bhat: I should probably explain how a ~= forall r. ( a-> r) -> r first?
[11:32 AM] Siddharth Bhat: ie, since bool = true | false, we get two parameters, one r for true, one r for false: boolcont := forall r. r [for true] -> r[for false] -> r.
[11:33 AM] Aditya Bharti:
I should probably explain how a ~= forall r. ( a-> r) -> r first?
@Siddharth Bhat nope, that much I got
[11:33 AM] Aditya Bharti: You lost me at "massage the type to get ..."
[11:33 AM] Siddharth Bhat: OK. does the bool thing make sense?
[11:33 AM] Siddharth Bhat:
boolcont := forall r. (bool -> r) -> r -- (1)
 = forall r. (r, r) -> r [bool -> r ~= r^|bool| ~= (r, r)] -- (2)
 = forall r. r -> r -> r [currying: (a, b) -> c ~= a -> b -> c] -- (3)


does the above derivation make sense?
[11:33 AM] Aditya Bharti: Nope.
[11:33 AM] Siddharth Bhat: I'm using identities of types to convert from one representation to another isomorphic representation
[11:34 AM] Siddharth Bhat: OK, which step doesn't make sense
[11:34 AM] Aditya Bharti: So this is natcont, which is fine natcont: forall r. (nat -> r) -> r
[11:34 AM] Siddharth Bhat: indeed.
[11:34 AM] Aditya Bharti: Then this is boolcont: boolcont: forall r. (bool -> r) -> r
[11:34 AM] Siddharth Bhat: yeah
[11:35 AM] Aditya Bharti: How did you massage this into the behemoth above?
[11:35 AM] Siddharth Bhat: well, like I wrote in step 2
[11:35 AM] Aditya Bharti: And in coq what does multiple definitions ( := ) mean?
[11:35 AM] Siddharth Bhat: let me change that to =
[11:35 AM] Siddharth Bhat: I meant that (1) is equal to (2)
[11:36 AM] Aditya Bharti: oh boy
[11:36 AM] Aditya Bharti: Let me re-examine the entire thing
[11:36 AM] Aditya Bharti: one moment
[11:36 AM] Siddharth Bhat: mmhm
[11:37 AM] Aditya Bharti:
ie, since bool = true | false, we get two parameters, one r for true, one r for false: boolcont := forall r. r [for true] -> r[for false] -> r.
@Siddharth Bhat this I don't get
[11:37 AM] Siddharth Bhat: OK
[11:37 AM] Siddharth Bhat: you understand till step  (3) , yes?
[11:38 AM] Aditya Bharti: Which message are you referring to?
[11:38 AM] Aditya Bharti: let's step back a bit
[11:38 AM] Aditya Bharti: As of now, here is what I got
[11:38 AM] Siddharth Bhat:
boolcont := forall r. (bool -> r) -> r -- (1)
 = forall r. (r, r) -> r [bool -> r ~= r^|bool| ~= (r, r)] -- (2)
 = forall r. r -> r -> r [currying: (a, b) -> c ~= a -> b -> c] -- (3)
[11:39 AM] Aditya Bharti: Right ok so I only got step (1)
[11:39 AM] Aditya Bharti: How did you replace (bool -> r) with the pair (r,r)?
[11:40 AM] Siddharth Bhat: so, if you give me f: bool -> r, this is ismorphic to the tuple (f true, f false) ?
[11:40 AM] Aditya Bharti: Yup ok.
[11:41 AM] Siddharth Bhat: and we know that (r, r) -> r is isomorphic to r -> r -> r by currying?
[11:41 AM] Aditya Bharti: ah (r, r) is a pair of the same type r
[11:41 AM] Aditya Bharti: my bad
[11:42 AM] Aditya Bharti:
and we know that (r, r) -> r is isomorphic to r -> r -> r by currying?
@Siddharth Bhat yup
[11:42 AM] Siddharth Bhat: yeah I think in coq it's called r * r
[11:42 AM] Aditya Bharti: Yes
[11:42 AM] Aditya Bharti: I thought (r,r) meant repeating the same value
[11:43 AM] Aditya Bharti: ok go on, I got this much
[11:45 AM] Siddharth Bhat:
Definition boolcont1 := forall (r: Type), (bool -> r) -> r.
Definition boolcont2 := forall (r: Type), (r * r) -> r.
Definition boolcont3 := forall (r: Type), r -> r -> r.

Definition use_boolcont1 (b: bool): boolcont1.
Proof.unfold boolcont1. intros r buser. exact (buser b). Qed.

Print use_boolcont1.
(* use_boolcont1 = 
  fun (b : bool) (r : Type) (buser : bool -> r) 
    => buser b
     : bool -> boolcont1
*)

Definition use_boolcont3 (b: bool): boolcont3.
Proof. unfold boolcont3. intros r rtrue rfalse.
       destruct b.
       + (* b = true *) exact rtrue.
       + (* b = false *) exact rfalse.
Qed.

Print use_boolcont3.
(* use_boolcont3 = 
  fun (b : bool) (r : Type) (rtrue rfalse : r)
    => if b then rtrue else rfalse
     : bool -> boolcont3
*)
[11:46 AM] Siddharth Bhat: so I implemented how to convert from bool to boolcont1, from bool to boolcont3.
[11:47 AM] Siddharth Bhat: does the implementation make sense?
[11:47 AM] Siddharth Bhat: (I wrote it in proof mode so you can think about it step-by-step. I wrote the commented output from Print for the full view)
[11:48 AM] Aditya Bharti: I'm stil uneasy about proving definitions, since, definitions cannot be false, in my head
[11:48 AM] Siddharth Bhat: @Aditya Bharti 
Definition x: 1 = 2. Proof. Abort.
[11:49 AM] Aditya Bharti: yes, but this is an equality right?
[11:50 AM] Aditya Bharti: You're just renaming, not sure what's there to prove about that.
[11:50 AM] Siddharth Bhat: how do you know that boolcont1 is inhabited?
[11:50 AM] Siddharth Bhat: consider this:
[11:50 AM] Siddharth Bhat:
Definition amiinhabited: forall (X Y:Type), X -> Y. Proof. Abort.
[11:50 AM] Siddharth Bhat: is amiinhabited inhabited/
[11:50 AM] Siddharth Bhat: it's not an equality.
[11:51 AM] Aditya Bharti: What do you mean inhabited?
[11:51 AM] Siddharth Bhat: can you give a Proof for it hat will let you write a Qed at the end
[11:51 AM] Aditya Bharti: You're asking me, does there exist a function from X -> Y?
[11:51 AM] Siddharth Bhat: can you construct a value of the type
[11:51 AM] Siddharth Bhat: yeah
[11:51 AM] Siddharth Bhat: just like what it means to prove boolcon1 is to give a value of the type forall (r: Type), (bool -> r) -> r
[11:52 AM] Siddharth Bhat: proving amiinhabited is providing a value of the type forall (X Y:Type), X -> Y
[11:53 AM] Siddharth Bhat: so, is it inhabited? :smile:
[11:53 AM] Aditya Bharti: I mean, from coq perspective, I'm not sure how to prove it. But from a math perspective, yes. There do exist functions from X -> Y for any types X and Y.
[11:53 AM] Aditya Bharti: Yes.
[11:53 AM] Siddharth Bhat: for any types under my control?
[11:53 AM] Siddharth Bhat: can you give a uniform defintion
[11:53 AM] Siddharth Bhat: that will work for any choice of mine of X and Y?
[11:54 AM] Aditya Bharti: As long you have at least one value of types X and Y, it should be possible.
[11:54 AM] Siddharth Bhat: I think you should try to write the above in coq
[11:54 AM] Siddharth Bhat:
  X : Type
  Y : Type
  X0 : X
  ============================
  Y
[11:54 AM] Siddharth Bhat: this is your proof state
[11:54 AM] Siddharth Bhat: how will you produce a Y?
[11:54 AM] Aditya Bharti:
that will work for any choice of mine of X and Y?
@Siddharth Bhat no. my math construction involves axiom of choice at multiple stages.
[11:55 AM] Aditya Bharti: I cannot produce a Y. but math tells me that there must be a procedure for constructing Y.
[11:55 AM] Anurudh Peduri: Y could be empty right?
[11:55 AM] Aditya Bharti: It can even be a constant function
[11:55 AM] Aditya Bharti:
As long you have at least one value of types X and Y, it should be possible.
@Anurudh Peduri
[11:56 AM] Anurudh Peduri: yeah, but he asked for any X and Y
[11:56 AM] Siddharth Bhat: @Aditya Bharti yeah, so it doesn't work for any choice of X and Y
[11:56 AM] Anurudh Peduri: so even false propositions (equivalent to empty sets/types) are allowed to be Y
[11:57 AM] Aditya Bharti: hmmm...
[11:57 AM] Aditya Bharti: So you're contructing values of type boolcont ...
[11:57 AM] Siddharth Bhat: as the definition states:

Definition amiinhabited: forall (X Y:Type), X -> Y.

forall is indeed forall, not for all that are provable. :slight_smile:
[11:57 AM] Aditya Bharti: ... or proofs of proposition boolcont?
[11:57 AM] Siddharth Bhat: what's the difference :smile:
[11:57 AM] Aditya Bharti: exactly my point
[11:57 AM] Siddharth Bhat: indeed
[11:57 AM] Aditya Bharti: It just clicked
[11:58 AM] Aditya Bharti: curry howard still boggles my mind
[11:58 AM] Siddharth Bhat: mm
[11:58 AM] Siddharth Bhat: it's curry-howard-lambek BTW :stuck_out_tongue:
[11:58 AM] Siddharth Bhat: idk why but people tend to drop the third dude :frowning:
[11:58 AM] Siddharth Bhat: anyway
[11:58 AM] Siddharth Bhat: citations aside.
[11:58 AM] Aditya Bharti: I've only heard you say curry howard.
[11:58 AM] Aditya Bharti: yeah
[11:59 AM] Siddharth Bhat: right, so we know that boolcont1 and boolcont3 are sensible
[12:00 PM] Siddharth Bhat: We can in fact prove that boolcont3 is isomorphic to bool
[12:04 PM] Siddharth Bhat: hmm
[12:04 PM] Siddharth Bhat: OK, doing the proofs in coq is getting into stuff that's not strictly necessary at this point
[12:04 PM] Siddharth Bhat: but I'll do it anyway
[12:10 PM] Siddharth Bhat: mh, OK
[12:10 PM] Siddharth Bhat: this is taking me too far away from the point
[12:10 PM] Siddharth Bhat: I can show one side of the isomorphism easily
[12:10 PM] Siddharth Bhat:
Definition bool_to_boolcont3 (b: bool): boolcont3.
Proof. unfold boolcont3. intros r rtrue rfalse.
       destruct b.
       + (* b = true *) exact rtrue.
       + (* b = false *) exact rfalse.
Defined.


Definition boolcont3_to_bool (bc: boolcont3): bool := bc bool true false.

Lemma iso_fwd: forall (b: bool), boolcont3_to_bool(bool_to_boolcont3 b) = b.
Proof.
  intros.
  unfold boolcont3_to_bool.
  unfold bool_to_boolcont3.
  destruct b; auto.
Qed.
[12:10 PM] Siddharth Bhat: The backward direction is harder, because you need to rely on classical logic
[12:11 PM] Siddharth Bhat: but OK, we believe that bool = forall r. (bool -> r) -> r = forall r. r -> r -> r.
[12:11 PM] Siddharth Bhat: right?
[12:11 PM] Siddharth Bhat: @Aditya Bharti
[12:11 PM] Siddharth Bhat: because intuitively,
- a boolean is a choice between true and false
[12:11 PM] Siddharth Bhat: - each choice corresponds to one value of r that we pass into the function, which is "picked" correctly by the right branch.
[12:12 PM] Siddharth Bhat: @Aditya Bharti yes/no/maybe?
[12:15 PM] Siddharth Bhat: @Aditya Bharti ping me when you're back, I'll continue and relate this to nat
[12:15 PM] Siddharth Bhat: we have seen how disjoint unions transform into different parameters in the continuation representation
[12:15 PM] Siddharth Bhat: we will next see how to handle recursion by considering list a := cons a (list) a| nil
[12:15 PM] Aditya Bharti: ping
[12:15 PM] Siddharth Bhat: we will then show how this naturally also leads to nat a := z | s nat
[12:15 PM] Aditya Bharti: so I'm looking at the isomorphism now
[12:16 PM] Siddharth Bhat: to be equivalent to forall r. (r -> r) ->  r -> r
[12:16 PM] Siddharth Bhat: OK
[12:18 PM] Aditya Bharti: So fine, the isomorphism is cool with me.
[12:18 PM] Aditya Bharti: What's next.
[12:19 PM] Siddharth Bhat: next, we'll study lists
[12:27 PM] Siddharth Bhat: mh
[12:27 PM] Siddharth Bhat: I'm trying to see what the most "natural" way to show this is
[12:27 PM] Siddharth Bhat: I "know the answer" so to speak
[12:28 PM] Siddharth Bhat: gimme a bit
[12:54 PM] Siddharth Bhat:
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types #-}

data NatF f = ZF | SF f deriving(Show, Functor)
data Mu f = Mu { unMu :: f (Mu f) }

instance (Show (Mu f),  Show (f (Mu f))) => Show (Mu f) where
    show (Mu x) = show x
type Nat = Mu NatF 
data Peano = Z | S Peano deriving(Show)

p2n :: Peano -> Nat
p2n Z = Mu ZF
p2n (S n) = Mu $ SF (p2n n)

n2p :: Nat -> Peano
n2p (Mu ZF) = Z
n2p (Mu (SF x)) = S (n2p x)



knatF :: NatF f -> (forall r. r -> (f -> r) -> r)
knatF ZF zfuser sfuser = zfuser
knatF (SF f) zfuser sfuser = sfuser f

kmu :: Mu f -> (forall r. (Mu f -> r) -> r)
kmu mu muuser = muuser mu


kmu' :: Functor f => Mu f -> (forall r. (f r -> r) -> r)
kmu' (Mu fmuf) muuser = muuser $ fmap (\mu -> kmu' mu muuser) fmuf


The TLDR is we build Mu which is the fixpoint combinator on the datatype level. We see that the continuation version of it when f is a Functor is of the type (f r -> r) -> r, suggesting that we can simply "normalize" the inner layers to be an r. Hence, when we writeNat  as the fixpoint of a functor NatF, we can simply replace all occurences of f with an r
[12:54 PM] Siddharth Bhat: so I don't have the bandwidth to explain this till the exams end :stuck_out_tongue:
[12:54 PM] Siddharth Bhat: but this contains the story of "how" if you care. You can google things like "haskell <mu>" and whatnot to reconstruct the story of "why the conversion is so"
[12:55 PM] Siddharth Bhat: I will provide scam intuition for now
[12:55 PM] Siddharth Bhat: and fix this on 29th once everything is done
[12:55 PM] Siddharth Bhat: I tried doing it in Coq; I get universe inconsitencies.
[12:56 PM] Siddharth Bhat: so the scam is this: let's consider nat:

Inductive nat := NZ: nat | NS: nat -> nat.
(* k for 'k'ontinuation --- this is standard in haskell circles *)
(* r for the 'r'eturn value in (nat -> r) -> r *)
Definition knat := forall r, (nat -> r) -> r.
[12:57 PM] Siddharth Bhat: @Aditya Bharti so far so good?
[12:57 PM] Aditya Bharti: yup
[12:58 PM] Siddharth Bhat: OK
[12:58 PM] Siddharth Bhat: so let's do our standard thing, and write this as:
[1:02 PM] Siddharth Bhat:
Definition knat2 := forall r, r -> (nat -> r) -> r.
Definition nat_to_knat2 (n: nat): knat2 := fun r nzuser nsuser =>  
  match n with
  | NZ => nzuser
  | NS n' => nsuser n'
  end.

1. r is what we will do in case we see a NZ. nat -> r is what we will do when we see a NS.
[1:02 PM] Siddharth Bhat: so far so good, again?
[1:02 PM] Siddharth Bhat: I have expanded (nat -> r) -> r into (r -> (nat -> r) -> r)
[1:02 PM] Siddharth Bhat: by "peeling off" the NZ case and the NS case.
[1:04 PM] Aditya Bharti:
I have expanded (nat -> r) -> r into (r -> (nat -> r) -> r)
@Siddharth Bhat after this expansion, do you still need the forall r there?
[1:04 PM] Aditya Bharti: Acutally, you probably do. nvm
[1:04 PM] Siddharth Bhat: Yeah, because I need to be able to allow you to give me "any user of nat"
[1:04 PM] Siddharth Bhat: right?
[1:05 PM] Aditya Bharti: yup cool that works
[1:05 PM] Siddharth Bhat: OK
[1:05 PM] Siddharth Bhat: but notice that I am not using the "power" of recursion in nat_to_knat2
[1:05 PM] Siddharth Bhat: ie, it is only Definition, not Fixpoint
[1:05 PM] Siddharth Bhat: hmm, how would I use Fixpoint?
[1:06 PM] Siddharth Bhat: notice that inductively, I can use Fixpoint to convert the inner n': nat into an r
[1:06 PM] Siddharth Bhat: because nat_to_knat2 is essentially saying "I will tell you how I can use a nat to produce an r"
[1:06 PM] Siddharth Bhat: that's what a knat is: a  thing that can use a nat to produce an r
[1:09 PM] Siddharth Bhat: this suggests the natural definition:

  
Definition knat3 := forall r, r -> (r -> r) -> r.
Fixpoint nat_to_knat3 (n: nat): knat3 := fun r nzuser nsuser =>  
  match n with
  | NZ => nzuser
  | NS n' => nsuser (nat_to_knat3 n' r nzuser nsuser)
  end.
[1:09 PM] Siddharth Bhat: so far so good?
[1:10 PM] Siddharth Bhat: compare and contrast with:

Definition knat2 := forall r, r -> (nat -> r) -> r.
Definition nat_to_knat2 (n: nat): knat2 := fun r nzuser nsuser =>  
  match n with
  | NZ => nzuser
  | NS n' => nsuser n'
  end.
[1:10 PM] Siddharth Bhat: in nat_to_knat3, I am not passing n'. Rather, I am recursively processing it, using nat_to_knat3 on n'.
[1:11 PM] Siddharth Bhat: that's the only diff: n' |-> (nat_to_knat3 n' r nzuser nsuser)
[1:13 PM] Aditya Bharti: This makes sense, although I'll have to spend time to understand it better. :stuck_out_tongue:
[1:13 PM] Aditya Bharti: What were we doing again?
[1:13 PM] Aditya Bharti: Lists, right.
[1:14 PM] Siddharth Bhat: No, we're done
[1:14 PM] Siddharth Bhat: we don't need lists
[1:14 PM] Siddharth Bhat: now look at the type
[1:16 PM] Siddharth Bhat:
Definition knat3 := forall r, r -> (r -> r) -> r.
 = forall r, (r -> r) -> r -> r [moving arguments: (a->b->c) = (b->a->c)]
 = forall (X: Type), (X -> X) -> X -> X 
 = cnat
[1:16 PM] Siddharth Bhat: ^_^
[1:16 PM] Siddharth Bhat: so this is why cnat represents nat

