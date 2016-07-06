Require Import all_ssreflect.

Definition bool_Prop_equiv (P : Prop) (b : bool) := b = true <-> P.

Lemma test_bool_Prop_equiv b P : bool_Prop_equiv P b -> P \/ ~ P.
Proof.
case: b; case => hlr hrl.
  by left; apply: hlr.
by right => hP; move: (hrl hP).
Qed.

Lemma iffP_lr (P : Prop) (b : bool) :
  (P -> b) -> (b -> P) -> reflect P b.
Proof.
by move=> *; apply: (iffP idP).
Qed.

Lemma iffP_rl (P : Prop) (b : bool) :
  reflect P b -> ((P -> b) /\ (b -> P)).
Proof. by case: b; case=> p; split. Qed.

Lemma eqnP {n m : nat} :
  reflect (n = m) (eqn n m).
Proof.
apply: (iffP idP) => [|->]; last by elim: m.
by elim: n m => [[]|n IH [//|m] /IH ->].
Qed.

Lemma nat_inj_eq T (f : T -> nat) x y :
  injective f ->
    reflect (x = y) (eqn (f x) (f y)).
Proof. by move=> f_inj; apply: (iffP eqnP) => [/f_inj|-> //]. Qed.

Lemma leq_max m n1 n2 : (m <= maxn n1 n2) = (m <= n1) || (m <= n2).
Proof.
wlog le_n21: n1 n2 / n2 <= n1 => [th_sym|].
  by case/orP: (leq_total n2 n1) => /th_sym; last rewrite maxnC orbC.
by rewrite (maxn_idPl le_n21) orb_idr // => /leq_trans->.
Qed.

Lemma edivnP m d :
  let ed := edivn m d in
  ((d > 0) ==> (ed.2 < d)) && (m == ed.1 * d + ed.2).
Proof.
move E: (edivn m d) => ed /=.
case: d => [|d /=] in E *; first by rewrite -E eqxx.
rewrite /edivn /= in E; rewrite -[m]/(0 * d.+1 + m).
elim: m {-2}m 0 (leqnn m) E => [|n IHn] [??<-|m] //= q le_mn.
rewrite subn_if_gt; case: ifP => [le_dm|lt_md <- /=]; last first.
  by rewrite ltnS ltnNge lt_md eqxx.
have le_mdn : m - d <= n by rewrite (leq_trans (leq_subr d m)).
move=> /(IHn _ _ le_mdn); rewrite mulSnr -addnA -subSS subnKC.
Qed.

Lemma dvdn_fact m n : 0 < m <= n -> m %| n`!.
Proof.
case: m => //= m; elim: n => //= n IHn; rewrite ltnS leq_eqVlt.
by case/orP=> [/eqP-> | /IHn]; [apply: dvdn_mulr | apply: dvdn_mull].
Qed.

Lemma prime_above m : exists2 p, m < p & prime p.
Proof.
have /pdivP[p pr_p p_dv_m1]: 1 < m`! + 1 by rewrite addn1 ltnS fact_gt0.
exists p => //; rewrite ltnNge; apply: contraL p_dv_m1 => p_le_m.
by rewrite dvdn_addr ?dvdn_fact ?prime_gt0 // gtnNdvd ?prime_gt1.
Qed.