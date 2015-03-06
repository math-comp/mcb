Require Import ssreflect ssrfun ssrnat.

Inductive nat : Type := O | S of nat.

Section Iter.

Variable A : Type.
Variable f : A -> A.

(* Version simple sans let in *)
Fixpoint iter n  (x : A) : A :=
  if n is S n1 then f (iter n1 x) else x.

Lemma iterS n x : iter (S n) x = f (iter n x).
Proof. by []. Qed.

End Iter.

Definition addn (m : nat) := iter nat S m.

Lemma add0n n : addn O n = n.
Proof. by []. Qed. 

Lemma addSn m n : addn (S m) n = S (addn m n).
Proof. by []. Qed. 

Definition muln (m : nat) (n : nat) := iter nat (addn n) m O.

Lemma muln0 n : muln O n = O.
Proof. by []. Qed.

Lemma mulSn m n : muln (S m) n = addn n (muln m n).
Proof. by []. Qed.

Definition expn (m : nat) (n : nat) := iter nat (muln m) n (S O).

Lemma expn0 m : expn m O = (S O).
Proof. by []. Qed. 

Lemma expnS m n : expn m (S n) = muln m (expn m n).
Proof. by []. Qed.
