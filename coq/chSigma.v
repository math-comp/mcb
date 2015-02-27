Require Import ssreflect ssrbool ssrfun eqtype ssrnat seq.

Set Implicit Arguments.

Module Section2.

Record tuple_of n T := Tuple { tval :> seq T; _ : size tval = n }.
Notation "n .-tuple T" := (tuple_of n T) (at level 2).
Arguments Tuple {n T _} _.

Lemma size_tuple T n (t : n.-tuple T) : size t = n.
Proof. by case: t. Qed.

Example seq_on_tuple n (t : n.-tuple nat) :
  size (rev [seq 2 * x | x <- rev t]) = size t.
Proof. by rewrite map_rev revK size_map. Qed.

Example just_tuple_attempt n (t : n.-tuple nat) :
  size (rev [seq 2 * x | x <- rev t]) = size t.
Proof. rewrite !size_tuple. Admitted.

Notation "X (*...*)" := (let x := X in let y := _ in x)
  (at level 100, format "X  (*...*)").

Notation "[LHS 'of' equation ]" :=
  (let LHS := _ in
   let _infer_LHS := equation : LHS = _ in LHS)
  (at level 4).

Notation "[unify X 'with' Y ]" :=
  (let unification := erefl _ : X = Y in
   True).

Fail Check forall T n (t : n.-tuple T),
  let LHS := [LHS of size_tuple _] in
  let RHS := size (rev t) in
  [unify LHS with RHS].

(* Canonical *)
Lemma rev_tupleP n T (t : n.-tuple T) : size (rev t) = n.
Proof. by rewrite size_rev size_tuple. Qed.
Canonical rev_tuple n T (t : n.-tuple T) := Tuple (rev_tupleP t).

Lemma map_tupleP T' n T (f: T -> T') (t : n.-tuple T) :
  size (map f t) = n.
Proof. by rewrite size_map size_tuple. Qed.
Canonical map_tuple T' n T (f : T -> T') (t : n.-tuple T) :=
  Tuple (map_tupleP f t).

Example just_tuple n (t : n.-tuple nat) :
  size (rev [seq 2 * x | x <- rev t]) = size t.
Proof. by rewrite !size_tuple. Qed.

(* Also *)
Canonical nil_tuple T := Tuple (erefl : @size T [::] = 0).

Lemma cons_tupleP n T (t : n.-tuple T) x : size (x :: t) = n.+1.
Proof. by rewrite /= size_tuple. Qed.
Canonical cons_tuple n T x (t : n.-tuple T) :=
  Tuple (cons_tupleP t x).

End Section2.













Implicit Type n : nat.

Record tuple_of n T := Tuple { tval :> seq T; _ : size tval == n }.
Notation "n .-tuple T" := (tuple_of n T) (at level 2).
Arguments tval {n T} _.

Definition tcmp (T : eqType) n (t1 t2 : n.-tuple T) :=
  tval t1 == tval t2.

Lemma eqtupleP (T : eqType) n : Equality.axiom (tcmp T n).
Proof.
move=> x y; apply: (iffP eqP); last first.
  by move->.
case: x; case: y => s1 p1 s2 p2 /= E.
rewrite E in p2 *.
by rewrite (eq_irrelevance p1 p2).
Qed.

Lemma xxx : forall  t : 3.-tuple nat, 7 \in rcons (tval t) 7.
move=> t; rewrite mem_rcons.

Canonical tuple_eqType T n :=
Eval hnf in Equality.pack (Equality.Mixin (eqtupleP T n)).


Lemma size_tuple (T : eqType) n (t : tuple_of T n) : size t = n.
Proof. by case: t => [s size_s]; rewrite (eqP size_s). Qed.

