From mathcomp Require Import all_ssreflect.

(* statements *)
Check 3 = 3.
Check false || true = true.
Locate "=".
About eq.
Fail Check 3 = [:: 3].

(* proofs *)
Lemma my_first_lemma : 3 = 3.
Proof. by []. Qed.
Lemma my_second_lemma : 2 + 1 = 3.
Proof. by []. Qed.
Lemma addSn m n : m.+1 + n = (m + n).+1.
Proof. by []. Qed.

Lemma negbK b : ~~ (~~ b) = b.
Proof. by case: b. Qed.

Lemma leqn0 n : (n <= 0) = (n == 0).
Proof. by case: n => [| k]. Qed.

Lemma muln_eq0 m n : (m * n == 0) = (m == 0) || (n == 0).
Proof.
case: m => [|m] //.
case: n => [|k] //.
by rewrite muln0.
Qed.

Lemma leqE m n : (m <= n) = (m - n == 0).
Admitted.
Lemma leq_mul2l m n1 n2 : (m * n1 <= m * n2) = (m == 0) || (n1 <= n2).
Proof. by rewrite !leqE -mulnBr muln_eq0. Qed.

(* quantifiers *)
Section StandardPredicates.
Variable T : Type.
Implicit Types (op add : T -> T -> T) (R : rel T).
Definition associative op := forall x y z, op x (op y z) = op (op x y) z.
Definition left_distributive op add :=
  forall x y z, op (add x y) z = add (op x z) (op y z).
Definition left_id e op := forall x, op e x = x.
End StandardPredicates.

Section MoreStandardPredicates.
Variables rT aT : Type.
Implicit Types (f : aT -> rT).
Definition injective f := forall x1 x2, f x1 = f x2 -> x1 = x2.
Definition cancel f g := forall x, g (f x) = x.
Definition pcancel f g := forall x, g (f x) = Some x.
End MoreStandardPredicates.

Lemma dvdn_fact m n : 0 < m -> m <= n -> m %| n`!.
Proof. by move=> m_gt0 ?; rewrite dvdn_fact ?m_gt0. Qed.

Lemma example m p : prime p ->
  p %| m `! + 1 -> m < p.
Proof.
move=> prime_p.
apply: contraLR.
rewrite -leqNgt.
move=> leq_p_m.
rewrite dvdn_addr.
  rewrite gtnNdvd.
About contraLR.
    by [].  (* ~~ false *)
    by [].  (* 0 < 1 *)
  by apply: prime_gt1.  (* 1 < p *)
apply: dvdn_fact.
  by apply: prime_gt0. (* 0 < p *)
by []. (* p <= m *)
Qed.

Lemma example1 m p : prime p ->
  p %| m `! + 1 -> ~~ (p <= m).
Proof.
move=> prime_p; apply: contraL; move=> leq_p_m.
rewrite dvdn_addr.
  by rewrite gtnNdvd // prime_gt1.
by rewrite dvdn_fact // prime_gt0.
Qed.

Lemma example2 m p : prime p ->
  p %| m `! + 1 -> ~~ (p <= m).
Proof.
move=> prime_p; apply: contraL; move=> leq_p_m.
rewrite dvdn_addr.
  by rewrite gtnNdvd // prime_gt1.
by rewrite dvdn_fact // prime_gt0.
Qed.

Lemma foldl_rev T A f (z : A) (s : seq T) :
 foldl f z (rev s) = foldr (fun x z => f z x) z s .
Proof.
elim/last_ind: s z => [|s x IHs] z //.
by rewrite -cats1 foldr_cat -IHs cats1 rev_rcons.
Qed.

(** Exercises *)

Lemma orbA b1 b2 b3 : b1 || (b2 || b3) = b1 || b2 || b3.
Proof. by case: b1; case: b2; case: b3. Qed.
Lemma implybE a b : (a ==> b) = ~~ a || b.
Proof. by case: a; case: b. Qed.
Lemma negb_and (a b : bool) : ~~ (a && b) = ~~ a || ~~ b.
Proof. by case: a; case: b. Qed.


Lemma subn_sqr m n : m ^ 2 - n ^ 2 = (m - n) * (m + n).
Proof. by rewrite mulnBl !mulnDr addnC [m * _]mulnC subnDl !mulnn. Qed.


Lemma odd_exp m n : odd (m ^ n) = (n == 0) || odd m.
Proof.
elim: n => // n IHn.
rewrite expnS odd_mul {}IHn orbC.
by case: odd.
Qed.

Definition all_words n T (alphabet : seq T) :=
  let prepend x wl := [seq x :: w | w <- wl] in
  let extend wl := flatten [seq prepend x wl | x <- alphabet] in
  iter n extend [::[::]].
Arguments all_words _ [T] _.


Lemma size_all_words n T (alphabet : seq T) :
  size (all_words n alphabet) = size alphabet ^ n.
Proof.
elim: n => [|n IHn]; first by rewrite expn0.
rewrite expnS -{}IHn [in LHS]/all_words iterS -/(all_words _ _).
elim: alphabet (all_words _ _) => //= w ws IHws aw.
by rewrite size_cat IHws size_map mulSn.
Qed.
