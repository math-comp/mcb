From mathcomp Require Import all_ssreflect.
Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Module Chapter72.
  Definition tcmp n (T : eqType) (t1 t2 : n.-tuple T) := tval t1 == tval t2.

  Lemma eqtupleP n (T : eqType) : Equality.axiom (@tcmp n T).
  Proof.
    move => x y; apply: (iffP eqP); last first.
      by move => ->.
      case: x; case y => s1 p1 s2 p2 /= E.
      rewrite E in p2 *.
        by rewrite (eq_irrelevance p1 p2).
  Qed.

  Canonical tuple_eqTuple n T : eqType :=
    Equality.Pack (Equality.Mixin (@eqtupleP n T)).

  Check forall t : 3.-tuple nat, [:: t] == [::].
  Check forall t : 3.-tuple bool, uniq [:: t; t].
  Fail Check forall t : 3.-tuple (7.-tuple nat), undup_uniq [:: t; t].

  Canonical tuple_subType n s := [subType for (@tval n s)].
  Fail Definition tuple_eqMixin n T := [eqMixin of n.-tuple T by <:].
  Canonical tuple_eqType n (T : eqType) := EqType (n.-tuple T) (@tuple_eqMixin n T).
End Chapter72.

Module Chapter721.
  Canonical tuple_subType n T := Eval hnf in [subType for (@tval n T)].
  Check tuple_subType.
  Check tval.

  Variables (s : seq nat) (t : 3.-tuple nat).
  Variables size3s : size s == 3.
  Let t1 : 3.-tuple nat := Sub s size3s.
  Let t2 := if insub s is Some t then val (t : 3.-tuple nat) else nil.
  Let t3 := insubd t s. (* 3.-tuple nat *)

  Section SubTypeKit.
    Variables (T : Type) (P : pred T).

    Structure subType : Type := SubType {
      sub_sort :> Type;
      val : sub_sort -> T;
      Sub : forall x, P x -> sub_sort;
      (* elimination rule for sub_sort *)
      _ : forall K (_ : forall x Px, K (@Sub x Px)) u, K u;
      _ : forall x Px, val (@Sub x Px) = x
    }.
  End SubTypeKit.

  Notation "[ 'subType' 'for' v ]" := (SubType _ v _
    (fun K K_S u => let (x, Px) as u return K u := u in K_S x Px)
    (fun x px => erefl x)).
End Chapter721.

Module Chapter722.
  Theorem eq_irrelevance (T : eqType) (x y : T) : forall e1 e2 : x = y, e1 = e2.
  Proof. Admitted.
End Chapter722.
