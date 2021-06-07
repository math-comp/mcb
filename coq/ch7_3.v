From mathcomp Require Import all_ssreflect.
Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Notation count_mem x := (count [pred y | y == x]).

Module finite.
  Definition axiom (T : eqType) (e : seq T) :=
    forall x : T, count_mem x e = 1.

  Record mixin_of (T : eqType) := Mixin {
    enum : seq T;
    _ : @axiom T enum;                                  
  }.
End finite.

(* Definition mytype_finMixin := Finite.Mixin mytype_enum mytype_enumP. *)
(* Canonical mytype_finType := @Finite.Pack mytype mytype_finMixin. *)

(* Lemma myenum_uniq : uniq myenum. *)
(* Lemma mem_myenum : forall x : T, x \in myenum. *)
(* Definition mytype_finMixin := Finite.UniqFinMixin myenum_uniq mem_myenum. *)

Lemma cardT (T : finType) : #|T| = size (enum T).
Proof. Admitted.
Lemma forallP (T : finType) (P : pred T) : reflect (forall x, P x) [forall x, P x].
Proof. Admitted.
