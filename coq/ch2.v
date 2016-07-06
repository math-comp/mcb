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