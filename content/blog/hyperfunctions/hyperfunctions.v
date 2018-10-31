Require Import ZArith.

(** Zip Fusion with Hyperfuntions *)


(** L definition *)
CoInductive L {A: Type} {B: Type}: Type :=
| LCons: (A -> B) -> (@L A B) -> @L A B.

CoFixpoint Llift {A B: Type} (f: A -> B): @L A B :=
  LCons f (Llift f).

(** There's no way to implement this normally, so steal
the conor mc bride paper that shows how to embed
non total programs in Coq?
https://arxiv.org/pdf/1805.08059.pdf - One monad to prove them all
 *)
Fixpoint Lrun {A: Type} (l: @L A A ): A :=
  match l with
  | LCons f fs =>  f (Lrun fs )
  end.
