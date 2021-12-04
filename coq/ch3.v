From mathcomp Require Import all_ssreflect.


(* 3.1 Propositions as types, proofs as programs *)

Check 3 : nat.

Section CheckNat.

Variable n : nat.
Check n + n : nat.

Fail Check n + n : bool.

Check muln0 n : n * 0 = 0.

End CheckNat.

(* 3.2 Terms, types, sorts *)

Check 7 = 7 : Prop.
Check 7 = 9 : Prop.
Check nat : Set.

Set Printing Universes.

Check Type : Type.
Check Prop : Type.
Check Set : Type.

(* This definition is already imported. *)
(* Inductive seq (A : Type) := nil | cons (hd : A) (tl : seq A). *)

Check seq.

Variables (n m : nat) (l : seq nat).

Hypothesis neq0 : n = 0.
(* Variable neq0 : n = 0. *)
Variable t : nat -> Type.

Variable g : forall n : nat, t n.

Check forall x : nat, bool : Type.

Variable f : nat -> bool.

Check f 3 : bool.

Inductive testbool := ttrue | tfalse.

Fail Check f ttrue : bool.

Variable g' : forall n, t n.
Check g' 3 : t 3.

Definition bar (n : nat) : bool := n == 0.

Check bar : nat -> bool.

Definition bar' (n : nat) : nat := n.
Print bar'.
Print id.
Compute bar' 4.


(* 3.3 Propositions, implication, universal quantification *)

Lemma modus_ponens (A B : Prop) : (A -> B) -> A -> B.
Proof.
  move=> hAB hA.
  apply: hAB.
  exact: hA.
Qed.

Lemma modus_ponens' (A B : Prop) : (A -> B) -> A -> B.
Proof.
  move=> hAB hA.
  exact: (hAB hA).
Qed.

Lemma modus_ponens'' (A B : Prop) : (A -> B) -> A -> B.
Proof.
  exact: (fun hAB hA => hAB hA).
Qed.


(* 3.5 Inductive types *)

(* This definition is already imported. *)
(* Inductive nat : Set := O : nat | S (n : nat). *)

Unset Printing Notations.
Variable n' : nat.

Check O.
Check S O.
Check S (S (S O)).
Check S (S (S n')).

Definition non_zero n := if n is (S p) then true else false.

Fixpoint addn n m :=
  match n with
  | 0 => m
  | S p => S (addn p m)
  end.


(* 3.6 More connectives *)

(* These definitions are already imported. *)
(* Inductive and (A B : Prop) : Prop := conj (pa : A) (pb : B). *)
(* Notation "A /\ B" := (and A B). *)

(* Inductive prod (A B : Type) := pair (a : A) (b : B). *)

Definition proj1 A B (p : A /\ B) : A :=
  match p with conj a _ => a end.

About conj.

(* These definitions are already imported. *)
(* Inductive ex (A : Type) (P : A -> Prop) : Prop := *)
(*   ex_intro (x : A) (p : P x). *)
(* Notation "'exists' x : A , p" := (ex A (fun x : A => p)) (at level 200). *)

Inductive or (A B : Prop) : Prop := or_introl (a : A) | or_intror (b : B).
Notation "A \/ B" := (or A B).

Print or_ind.

(* Definition or_ind (A B P : Prop) *)
(*            (aob : A \/ B) (pa : A -> P) (pb : B -> P) : P := *)
(*   match aob with or_introl a => pa a | or_intror b => pb b end. *)

Inductive True : Prop := I.
Inductive False : Prop := .
Definition not (A : Prop) := A -> False.
Notation "~ A" := (not A).

Definition exfalso (P : Prop) (f : False) : P :=
  match f with end. (* no constructors, no branches *)

Inductive eq (A:Type) (x:A) : A -> Prop := erefl : eq A x x.
Notation "x = y" := (@eq _ x y).

Print eq_ind.

(* Definition eq_ind A (P : A -> Prop) x (px : P x) y (e : x = y) : P y := *)
(*   match e with erefl => px end. *)

About nat_ind.

Fixpoint nat_ind (P : nat -> Prop)
         (p0 : P 0) (pS : forall n : nat, P n -> P n.+1) n : P n :=
  if n is m.+1 then
    let pm (* : P m *) := nat_ind P p0 pS m in
    pS m pm (* : P m.+1 *)
  else p0.

Lemma strong_nat_ind (P : nat -> Prop)
      (base : P 0)
      (step : forall n, (forall m, m <= n -> P m) -> P n.+1) x : P x.
  Check (nat_ind (fun n => forall m, m <= n -> P m)).
  Proof.
    apply: (nat_ind (fun n => forall m, m <= n -> P m) _ _ x x (leqnn x)).
  Admitted.

Fail Fixpoint oops (n : nat) : False := oops n.
Fail Check oops 3. (* : False *)

Fail Inductive hidden := Hide (f : hidden -> False).
Fail Definition oops (hf : hidden) : False := let: Hide f := hf in f hf.
Fail Check oops (Hide oops). (* : False *)


(* Code below is not in the book anymore and given here as *)
(* additional examples.                                    *)

Lemma stamps n : 12 <= n -> exists s4 s5, s4 * 4 + s5 * 5 = n.
Proof.
have [m leq_mn] := ubnPgeq n; elim: n => // n IHn in m leq_mn *; first by case: n => [|n] in IHn *.
do 12! [ case: m => //= m in leq_mn * ] => _.
case: m => [|m] in leq_mn *; first by exists 3, 0.
case: m => [|m] in leq_mn *; first by exists 2, 1.
case: m => [|m] in leq_mn *; first by exists 1, 2.
case: m => [|m] in leq_mn *; first by exists 0, 3.
case: (IHn ((16+m) - 4) _ isT) => [|s4 [s5 def_m4]]. 
  by rewrite leq_subLR (leq_trans leq_mn) // addSnnS leq_addl.
by exists s4.+1, s5; rewrite mulSn -addnA def_m4 subnKC.
Qed.

Lemma absurd : false = true -> forall P, P.
Proof. by []. Qed.

Definition leq_trans n m o (Hmn : m <= n) (Hno : n <= o) : m <= o :=
  nat_ind (fun n => forall m o, m <= n -> n <= o -> m <= o)
    (fun m => match m with
      | 0 => fun o Hmn Hno => (isT : 0 <= o)
      | pm.+1 => fun o (Hmn : pm.+1 <= 0) Hno => absurd Hmn (pm.+1 <= o)
      end)
    (fun pn (IHn : forall m o : nat, m <= pn -> pn <= o -> m <= o) =>
      fun m => match m with
       | 0 => fun o Hmn Hno => (isT : 0 <= o)
       | pm.+1 => fun o (Hmn : pm.+1 <= pn.+1) => match o with
                   | 0 => fun (Hno : pn.+1 <= 0) => absurd Hno (pm.+1 <= 0)
                   | po.+1 => fun (Hno : pn.+1 <= po.+1) =>
                       IHn pm po (Hmn : pm <= pn) (Hno : pn <= po)
                   end
       end)
    n
  m o Hmn Hno.

Definition strong_nat_ind' (P : nat -> Prop)
  (base : P 0)
  (step : forall n, (forall m, m <= n -> P m) -> P n.+1) n : P n
:=
  nat_ind (fun n => forall m, m <= n -> P m)
    (fun m => match m with
      | 0 => fun Hmn => base
      | pm.+1 => fun (Hmn : pm.+1 <= 0) => absurd Hmn (P pm.+1)
      end)
    (fun pn (IHn : forall m, m <= pn -> P m) =>
      fun m => match m with
       | 0 => fun Hmn => base
       | pm.+1 => fun (Hmn : pm.+1 <= pn.+1) =>
            let P_upto_pm j (Hjm : j <= pm) : P j :=
              IHn j (leq_trans pm j pn Hjm (Hmn : pm <= pn)) in
            step pm P_upto_pm
       end)
    n
  n (leqnn n : n <= n).

Axiom P : nat -> Prop.
Check (nat_ind (fun n => forall m, m <= n -> P m)).

Lemma  strong_nat_ind2 (P : nat -> Prop)
  (base : P 0)
  (step : forall n, (forall m, m <= n -> P m) -> P n.+1) x : P x.
Proof.
apply: (nat_ind (fun n => forall m, m <= n -> P m) _ _ x x (leqnn x)).
  Show.
  by case=> [_| //]; exact: base.
move=> n IHn; case=> [_|m Hm]; first by exact: base.
apply: step=> j Hjm; apply: IHn.
apply: leq_trans Hjm Hm.
Qed.
