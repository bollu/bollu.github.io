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
  nth_error (filter (eq needle) xs) n
  
