From mathcomp Require Import all_ssreflect.
Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Module FinFunDef.
  Variable (aT : finType). (* domain type *)
  Variable (rT : Type). (* codomain type *)

  Inductive finfun_type : Type := Finfun of #|aT|.-tuple rT. 
  Definition finfun_of of phant (aT -> rT) := finfun_type.
  Definition fgraph f := let: Finfun t := f in t.

  Canonical finfun_subType := Eval hnf in [newType for fgraph].

  Notation "{ 'ffun' fT }" := (finfun_of (Phant fT)).
End FinFunDef.

Definition fun_of_fin aT rT f x := tnth (@fgraph aT rT f) (enum_rank x).
(* Coercion fun_of_fin : finfun >-> Funclass. *)
Definition finfun aT rT f := @Finfun aT rT (codom_tuple f).
Notation "[ ’ffun’ x : aT => F ]" := (finfun (fun x : aT => F)).  

Check [ffun i : 'I_4 => i + 2]. (* : {ffun 'I_4 -> nat} *)

(* Inductive tree3 := Leaf of nat | Node of {ffun 'I_3 -> tree3}. *)

(* Definition finfun_eqMixin aT (rT : eqType) := *)
(*   Eval hnf in [eqMixin of @finfun aT rT by <:]. *)
(* Canonical finfun_eqType := *)
(*   Eval hnf in EqType (finfun aT rT) finfun_eqMixin. *)

(* Definition tuple_enum (T : finType) n : seq (n.-tuple T) := *)
(*   pmap insub (all_words n (enum T)). *)

(* Lemma enumP T n : Finite.axiom (tuple_enum T n). *)

Definition tuple_finMixin n T := Eval hnf in FinMixin (@FinTuple.enumP n T).
Canonical tuple_finType n (T : finType) :=
  Eval hnf in FinType (n.-tuple T) (@tuple_finMixin n T).

(* Definition finfun_finMixin (aT rT : finType) := 
     [finMixin of (finfun aT rT) by <:]. *)
(* Canonical finfun_finType aT rT := 
     Eval hnf in FinType (finfun aT rT) (finfun_finMixin aT rT). *)

Lemma card_ffun (aT rT : finType) : #| {ffun aT -> rT} | = #|rT| ^ #|aT|.
Proof. Admitted.

Definition eqfun A B (f g : B -> A) : Prop := forall x, f x = g x.
Notation "f1 =1 f2" := (eqfun f1 f2).

(* Lemma ffunP aT rT (f1 f2 : {ffun aT -> rT}) : f1 =1 f2 <-> f1 = f2. *)

(* Lemma bigA_distr_bigA (I J : finType) F : *)
(*   \big[*%M/1]_(i : I) \big[+%M/0]_(j : J) F i j *)
(*   = \big[+%M/0]_(f : {ffun I -> J}) \big[*%M/1]_i F i (f i). *)
