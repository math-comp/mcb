From mathcomp Require Import all_ssreflect.

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
Implicit Arguments SubType [T P].

Notation "[ 'xsubType' 'for' v ]" :=
  (SubType _ v _ 
    (fun K K_S u => let (x, Px) as u return K u := u in K_S x Px)
    (fun x px => erefl x)).

Record foo := { v :> nat; p : v == 3 }.

Canonical xxx := Eval hnf in [xsubType for v].
