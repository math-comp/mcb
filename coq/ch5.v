From mathcomp Require Import all_ssreflect.


(* 5 Inductive specifications *)

Fixpoint has T (a : T -> bool) (s : seq T) : bool :=
  if s is x :: s' then a x || has _ a s' else false.

Definition has_prop T (a : T -> bool) (x0 : T) (s : seq T) :=
  exists i, i < size s /\ a (nth x0 s i).


(* 5.1.1 Relating statements in bool and Prop *)

Definition bool_Prop_equiv (P : Prop) (b : bool) := b = true <-> P.

Lemma test_bool_Prop_equiv b P : bool_Prop_equiv P b -> P \/ ~ P.
Proof.
  case: b; case => hlr hrl.
  by left; apply: hlr.
  by right => hP; move: (hrl hP).
Qed.

Inductive reflect1 (P : Prop) (b : bool) : Prop :=
| ReflectT1 (p : P) (e : b = true)
| ReflectF1 (np : ~ P) (e : b = false).

Lemma test_reflect b P : reflect P b -> P \/ ~ P.
Proof. case. Abort.

Lemma andP1 (b1 b2 : bool) : reflect (b1 /\ b2) (b1 && b2).
Proof. by case: b1; case: b2; [ left | right => //= [[l r]] ..]. Qed.

Lemma orP1 (b1 b2 : bool) : reflect (b1 \/ b2) (b1 || b2).
Proof.
  case: b1; case: b2; [ left; by [ move | left | right ] .. |].
  by right=> // [[l|r]].
Qed.

Lemma implyP1 (b1 b2 : bool) : reflect (b1 -> b2) (b1 ==> b2).
Proof.
  by case: b1; case: b2; [ left | right | left ..] => //= /(_ isT).
Qed.


(* 5.1.2 Proving reflection views *)

Lemma eqnP1 (n m : nat) : reflect (n = m) (eqn n m). Admitted.

About iffP.
(* iffP : forall [P Q : Prop] [b : bool], reflect P b -> (P -> Q) -> (Q -> P) -> reflect Q b *)

Lemma idP1 {b : bool} : reflect b b. Admitted.

Lemma eqnP2 {n m : nat} : reflect (n = m) (eqn n m).
  apply: (iffP idP).
Admitted.

Lemma nat_inj_eq1 T (f : T -> nat) x y :
  injective f -> reflect (x = y) (eqn (f x) (f y)).
Proof.
  move=> f_inj.
  apply: (iffP eqnP).
Admitted.


(* 5.1.3 Using views in intro patterns *)

Goal forall n m k, k <= n -> (n <= m) && (m <= k) -> n = k.
Proof.
  move => n m k lekn /andP.
Abort.
  
Lemma elimTF1 (P : Prop) (b c : bool) :
  reflect P b -> b = c -> if c then P else ~ P. Admitted.

Goal forall n m k, k <= n -> (n <= m) && (m <= k) -> n = k.
Proof.
  move=> n m k lekn /andP[lenm lemk].
Abort.

Lemma example n m k : k <= n -> (n <= m) && (m <= k) -> n = k.
Proof.
  move=> lekn /andP[/eqnP lenm lemk].
Abort.


(* 5.1.4 Using views with tactics *)

Goal forall m n, (m <= n) || (m >= n).
  move => m n; rewrite -implyNb.
  (* ~~ (m <= n) ==> (n <= m) *)
Abort.

Goal forall m n, (m <= n) || (m >= n).
  move => m n; rewrite -implyNb -ltnNge.
  (* (n < m) ==> (n <= m) *)
Abort.

Goal forall m n, (m <= n) || (m >= n).
  move => m n; rewrite -implyNb -ltnNge; apply/implyP.
  (* n < m -> n <= m *)
Abort.  

Lemma leq_total m n : (m <= n) || (m >= n).
Proof. by rewrite -implyNb -ltnNge; apply/implyP; apply: ltnW. Qed.

Goal forall m n1 n2, (m <= maxn n1 n2) = (m <= n1) || (m <= n2).
  move => m n1 n2; case/orP: (leq_total n2 n1) => [le_n21 | le_n12].
Abort.

Lemma maxn_idPl {m n} : reflect (maxn m n = m) (m >= n). Admitted.

Lemma leq_max m n1 n2 : (m <= maxn n1 n2) = (m <= n1) || (m <= n2).
Proof.
  case/orP: (leq_total n2 n1) => [le_n21 | le_n12].
    rewrite (maxn_idPl le_n21).
Admitted.

Inductive reflect (P : Prop) : bool -> Prop :=
| ReflectT (p : P) : reflect P true
| ReflectF (np : ~ P) : reflect P false.


Goal forall a b, a && b ==> (a == b).
  move => a b; case: andP => [ab|nab].
  (* true ==> (a == b) *)
  (* false => (a == b) *)
Abort.

Lemma example a b : a && b ==> (a == b).
Proof. by case: andP => [[-> ->] |]. Qed.

Lemma example1 a b : (a || ~~ a) && (a && b ==> (a == b)).
Proof. by case: (a && _) / andP => [[-> ->] |] //; rewrite orbN. Qed.

Section If.
  Variables (A : Type) (vT vF : A) (b : bool).

  Inductive if_spec : bool -> A -> Type :=
  | IfSpecTrue (p : b) : if_spec true vT
  | IfSpecFalse (p : b = false) : if_spec false vF.

  Lemma ifP : if_spec b (if b then vT else vF). Admitted.
End If.


(* 5.3 Strong induction via inductive specs *)

Inductive ubn_geq_spec m : nat -> Type :=
  UbnGeq n of n <= m : ubn_geq_spec m n.

Lemma ubnPgeq m : ubn_geq_spec m m.
Proof. by []. Qed.

Lemma test_ubnP (G : nat -> Prop) m : G m.
Proof.
  case: (ubnPgeq m).
Admitted.
