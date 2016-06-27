From mathcomp Require Import all_ssreflect.

Set Implicit Arguments.
Unset Strict Implicit.

(* pair *)

Inductive pair (A B : Type) := mk_pair (a : A) (b : B).

Notation "( A , B )" := (mk_pair A B).

Definition fst A B (p : pair A B) := let: (x, _) := p in x.

Eval compute in fst (4,5).

(* iter add *)
Definition addn n1 n2 := iter n1 S n2.

Eval compute in addn 3 4.

(* iter mul *)
Definition muln n1 n2 := iter n1 (addn n2) 0.

Eval compute in muln 3 4.

(* nth *)
Fixpoint nth T (def : T) (s : seq T) n :=
  if s is x :: s' then if n is n'.+1 then nth def s' n' else x else def.

Eval compute in nth 0 [:: 3; 7; 11; 22] 2.
Eval compute in nth 0 [:: 3; 7; 11; 22] 7.

(* rev *)
Fixpoint catrev T (s1 s2 : seq T) :=
  if s1 is x :: xs then catrev xs (x :: s2) else s2.

Definition rev T (s : seq T) := catrev s [::].

Eval compute in rev [:: 1; 2; 3].

(* flatten *)
Definition flatten T (s : seq (seq T)) := foldr cat [::] s.

Eval compute in
  flatten [:: [:: 1; 2; 3]; [:: 4; 5] ].

(* all_words *)
Definition all_words n T (alphabet : seq T) :=
  let prepend x wl := [seq x :: w | w <- wl] in
  let extend wl := flatten [seq prepend x wl | x <- alphabet] in
  iter n extend [::[::]].

Eval compute in all_words 2 [:: 1; 2; 3].
(*
 = [:: [:: 1; 1]; [:: 1; 2]; [:: 1; 3];
       [:: 2; 1]; [:: 2; 2]; [:: 2; 3];
       [:: 3; 1]; [:: 3; 2]; [:: 3; 3]]
 : seq (seq nat)
*)