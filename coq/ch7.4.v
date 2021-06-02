From mathcomp Require Import all_ssreflect.
Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Module Ordinal74.
  Inductive ordinal (n : nat) : Type := Ordinal m of m < n.
  Notation "''I_' n" := (ordinal n).

  Coercion nat_of_ord n (i : ordinal n) := let: Ordinal m _ := i in m.
  Canonical ordinal_subType n := [subType for (@nat_of_ord n)].
  Definition ordinal_eqMixin n := Eval hnf in [eqMixin of (@ordinal n) by <:].
  Canonical ordinal_eqType n :=
    Eval hnf in EqType (ordinal n) (ordinal_eqMixin n).

  Definition ord_enum n : seq (ordinal n) := pmap insub (iota 0 n).
End Ordinal74.

Lemma val_ord_enum n : map val (ord_enum n) = iota 0 n.
Proof.
  rewrite pmap_filter; last by exact: insubK.
    by apply/all_filterP; apply/allP => i; rewrite mem_iota isSome_insub.
Qed.

Lemma ord_enum_uniq n : uniq (ord_enum n).
Proof. by rewrite pmap_sub_uniq ?iota_uniq. Qed.

Lemma mem_ord_enum n i : i \in (ord_enum n).
Proof.
    by rewrite -(mem_map (@ord_inj n)) (val_ord_enum n) mem_iota ltn_ord.
Qed.

Definition ordinal_finMixin n :=
  Eval hnf in UniqFinMixin (ord_enum_uniq n) (@mem_ord_enum n).

Canonical ordinal_finType n :=
  Eval hnf in FinType (ordinal n) (ordinal_finMixin n).

Lemma tnth_default T n (t : n.-tuple T) : 'I_n -> T.
Proof. by rewrite -(size_tuple t); case (tval t) => [|//] []. Qed.

Definition tnth T n (t : n.-tuple T) (i : 'I_n) : T :=
  nth (tnth_default t i) t i.

Definition enum_rank (T : finType) : T -> 'I_#|T|.


