From mathcomp Require Import all_ssreflect.
Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Module DefTupleOf.
  Structure tuple_of n T := Tuple { tval :> seq T; _ : size tval == n }.
  Notation "n .-tuple T" := (tuple_of n T) (at level 2).

  Lemma size_tuple T n (t : n.-tuple T) : size t = n.
  Proof. by case: t => s /= /eqP. Qed.

  Example seq_on_tuple n (t : n.-tuple nat) :
    size (rev [seq 2 * x | x <- rev t]) = size t.
  Proof. by rewrite map_rev revK size_map. Qed.

  Example just_tuple_attempt n (t : n.-tuple nat) :
    size (rev [seq 2 * x | x <- rev t]) = size t.
  Proof. rewrite !size_tuple. Admitted.

  Notation "X (*...*)" := 
    (let x := X in let y := _ in x) (at level 100, format "X (*...*)").
  Notation "[LHS 'of' equation ]" := 
    (let LHS := _ in let _infer_LHS := equation : LHS = _ in LHS) (at level 4).
  Notation "[unify X 'with' Y ]" := 
    (let unification := erefl _ : X = Y in True).
  
  Fail Check forall T n (t : n.-tuple T),
      let LHS := [LHS of size_tuple _] in
      let RDX := size (rev t) in
      [unify LHS with RDX].  

  Variables (n : nat) (A B : Type).

  Lemma rev_tupleP (t : n.-tuple A) : size (rev t) == n.
  Proof. by rewrite size_rev size_tuple. Qed.

  Canonical rev_tuple (t : n.-tuple A) := Tuple (rev_tupleP t).

  Lemma map_tupleP (f : A -> B) (t : n.-tuple A) : size (map f t) == n.
  Proof. by rewrite size_map size_tuple. Qed.

  Canonical map_tuple (f : A -> B) (t : n.-tuple A) := Tuple (map_tupleP f t).

  Lemma cons_tupleP (t : n.-tuple A) x : size (x :: t) == n.+1.
  Proof. by rewrite /= size_tuple. Qed.

  Canonical cons_tuple x (t : n.-tuple A) : n.+1.-tuple A :=
    Tuple (cons_tupleP t x).
End DefTupleOf.

Example just_tuple n (t : n.-tuple nat) :
  size (rev [seq 2 * x | x <- rev t]) = size t.
Proof. by rewrite !size_tuple. Qed.
