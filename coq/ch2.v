Require Import all_ssreflect.

Lemma dvdn_fact m n : 0 < m -> m <= n -> m %| n`!.
Proof. by move=> m_gt0 ?; rewrite dvdn_fact ?m_gt0. Qed.

Lemma example m p : prime p ->
  p %| m `! + 1 -> ~~ (p <= m).
Proof.
move=> prime_p.
apply: contraL.
move=> leq_p_m.
rewrite dvdn_addr.
  rewrite gtnNdvd.
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

Lemma orbA b1 b2 b3 : b1 || (b2 || b3) = b1 || b2 || b3.
Proof. by case: b1; case: b2; case: b3. Qed.
Lemma implybE a b : (a ==> b) = ~~ a || b.
Proof. by case: a; case: b. Qed.
Lemma negb_and (a b : bool) : ~~ (a && b) = ~~ a || ~~ b.
Proof. by case: a; case: b. Qed.

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

