
From mathcomp Require Import all_ssreflect.

Example gauss n :
  \sum_(0 <= i < n.+1) i = n * n.+1 %/ 2.
Proof.
elim: n =>[|n IHn]; first by apply: big_nat1.
rewrite big_nat_recr //= IHn addnC -divnMDl //. 
by rewrite mulnS muln1 -addnA -mulSn -mulnS.
Qed.

Module Textbook.

Inductive nat : Type :=
  | Zero
  | Succ of nat
  | Plus of nat & nat.

Axiom plusZn : forall n, Plus Zero n = n.
Axiom plusSn : forall n m, Plus (Succ n) m = Succ (Plus n m).

Lemma plus23 :
  Plus (Succ (Succ Zero)) (Succ (Succ (Succ Zero))) =
  Succ (Succ (Succ (Succ (Succ Zero)))).
Proof.
rewrite plusSn.
rewrite plusSn.
rewrite plusZn.
reflexivity.
Qed.

Definition leq n m := exists d, Plus n d = m.

Lemma leq25 : leq (Succ (Succ Zero)) (Succ (Succ (Succ (Succ (Succ Zero))))).
Proof.
unfold leq.
exists (Succ (Succ (Succ Zero))).
exact: plus23.
Qed.

End Textbook.


Module CoqFolklore.

Inductive nat : Type :=
  | Zero
  | Succ of nat.

Fixpoint plus n m := if n is Succ n' then Succ (plus n' m) else m.

Eval simpl in fun n => plus Zero n.
Eval simpl in fun n m => plus (Succ n) m.

Definition leq n m := exists d, plus n d = m.

Lemma plus23 :
  plus (Succ (Succ Zero)) (Succ (Succ (Succ Zero))) =
  Succ (Succ (Succ (Succ (Succ Zero)))).
Proof.
reflexivity.
Qed.

Lemma leq25 : leq (Succ (Succ Zero)) (Succ (Succ (Succ (Succ (Succ Zero))))).
Proof.
unfold leq.
exists (Succ (Succ (Succ Zero))).
exact plus23.
Qed.

End CoqFolklore.


Module Ssr.

Inductive nat : Type :=
  | Zero
  | Succ of nat.

Fixpoint plus n m := if n is Succ n' then Succ (plus n' m) else m.

Fixpoint leq n m := if n is Succ n' then
                     if m is Succ m' then leq n' m' else false
                   else true.

Definition is_true b := b = true.

Lemma plus23 :
  plus (Succ (Succ Zero)) (Succ (Succ (Succ Zero))) =
  Succ (Succ (Succ (Succ (Succ Zero)))).
Proof.
reflexivity.
Qed.

Lemma leq25 :
  is_true (leq (Succ (Succ Zero)) (Succ (Succ (Succ (Succ (Succ Zero)))))).
Proof.
reflexivity.
Qed.

End Ssr.


From Ssreflect Require Import ssrfun ssrbool eqtype ssrnat.

Module Arith.

Print leq. (* !!! not is true, uses == *)
Check 0.
Check 3 - 2.
Eval compute in 3 - 2.
About "_ - _".
Eval compute in 3 - 2 == 1.
About "_ == _".


End Arith.
