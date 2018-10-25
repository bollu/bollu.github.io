
Require Import ZArith.
Require Import Omega.
Require Import List.
Require Import  Basics.
Open Scope list_scope.


Definition bool_to_nat (b: bool): nat :=
  match b with
  | false => 0
  | true => 1
end.


Definition sum (xs: list nat): nat :=
  fold_left plus xs 0.

(* num occurences of needle in [1..n] of xs *)
Fixpoint rank {X: Type}
         (eq: X -> X -> bool)
         (needle: X) (xs: list X)
         (n: nat): nat :=
  sum (map (compose bool_to_nat (eq needle)) (firstn n xs)).

Fixpoint zip_positions {X: Type} (xs: list X): list (nat * X) :=
  combine (seq 0 (length xs)) xs.
  
(* position of nth needle in xs *)
Fixpoint select {X: Type} (eq: X -> X-> bool) (needle: X) (xs: list X) (n: nat) : option nat :=
  option_map fst
             (nth_error
                (filter (fun posx => eq needle (snd posx))
                              (zip_positions xs)) n).


Theorem rank_select: forall {X: Type}
                       (eq: X -> X -> bool)
                       (needle: X)
                       (xs: list X)
                       (n sel: nat)
                       (SEL: select eq needle xs n = Some sel),
    rank eq needle xs sel = n.
Proof.
  intros until xs.
  (** need reverse induction principle **)
Abort.

Theorem select_rank:
  forall {X: Type}
    (eq: X -> X -> bool)
    (needle: X)
    (xs: list X)
    (n: nat),
    select eq needle xs (rank eq needle xs n) = Some n.
Proof.
Abort.

(* We have now established the adjunction between rank and select *)