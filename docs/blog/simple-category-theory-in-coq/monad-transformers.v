Require Import ZArith.

(** Encode categories *)
Record Category :=
  mkCategory {
      obj : Type;
      arr: obj -> obj -> Type;
      (** identity arrow *)
      idarr (c: obj): arr c c;
      (** arrow composition *)
      composearr {c1 c2 c3: obj}: arr c1 c2 -> arr c2 c3 -> arr c1 c3;
      (** composearr_assoc *)
      composearr_assoc:
        forall {c1 c2 c3 c4: obj}
          (a12: arr c1 c2)
          (a23: arr c2 c3)
          (a34: arr c3 c4),
          composearr a12 (composearr a23 a34) =
          composearr (composearr a12 a23) a34;
      (** left identity *)
      id_left: forall {c1 c2: obj} (a12: arr c1 c2),
          composearr (idarr c1) a12 = a12;
      (** Right identity *)
      id_right: forall {c1 c2: obj} (a12: arr c1 c2),
          composearr a12 (idarr c2) = a12;
    }.

(** Type with one inhabitant *)
Inductive Point := mkPoint.

Definition x : Category :=
   {|
      obj := Point;
      arr :=  fun (o1 o2: Point) => Point;
      idarr := forall (o: Point), mkPoint;
|}.


(** Encode functors *)
Record Functor {C D: Category} :=
  mkFunctor {
      functorObj: obj C -> obj D;
      functorArr: forall {c1 c2: obj C},
          arr C c1 c2 -> arr D (functorObj c1) (functorObj c2);
      (** commutative diagram *)
      functorCommute: forall (c1 c2: obj C) (a_c: arr C c1 c2)
                        (ISARR: List.In a_c (hom C c1 c2) ),
          List.In (functorArr a_c)
                (hom D (functorObj c1) (functorObj c2));
      
    }.

(** How in the fuck do I encode natural transformations in Coq? *)

(**

F c -- F a --> F c'
|               |
nat c           nat c'
|               |
G c --- G a --> G c'
*)
