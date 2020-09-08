From mathcomp Require Import all_ssreflect.

Module reflect1.

Inductive reflect (P : Prop) b :=
 | RT (p : P) (e : b)
 | RF (p : ~ P) (e : b = false).

Lemma andP (b1 b2 : bool) : reflect (b1 /\ b2) (b1 && b2).
Proof.
by case: b1; case: b2; [ left | right => //= [[l r]] ..].
Qed.

Lemma orP (b1 b2 : bool) : reflect (b1 \/ b2) (b1 || b2).
Proof.
case: b1; case: b2; [ left; by [ move | left | right ] .. |].
by right=> // [[l|r]].
Qed.

Lemma implyP (b1 b2 : bool) : reflect (b1 -> b2) (b1 ==> b2).
Proof.
by case: b1; case: b2; [ left | right | left ..] => //= /(_ isT).
Qed.

End reflect1.

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

Definition edivn_rec d := 
  fix loop m q := if m - d is m'.+1 then loop m' q.+1 else (q, m).

Definition edivn m d := if d > 0 then edivn_rec d.-1 m 0 else (0, m).

Lemma edivn_recE d m q :
  edivn_rec d m q =
    if m - d is m'.+1 then edivn_rec d m' q.+1 else (q, m).
Proof. by elim: m. Qed.

Lemma edivnP m d (ed := edivn m d) :
  ((d > 0) ==> (ed.2 < d)) && (m == ed.1 * d + ed.2).
Proof.
rewrite -[m]/(0 * d + m).
case: d => [//= | d /=] in ed *.
rewrite -[edivn m d.+1]/(edivn_rec d m 0) in ed *.
case: (ubnPgeq m) @ed; elim: m 0 => [|m IHm] q [/=|n] leq_nm //.
rewrite edivn_recE subn_if_gt; case: ifP => [le_dm ed|lt_md]; last first.
  by rewrite /= ltnS ltnNge lt_md eqxx.
rewrite -ltnS in le_dm; rewrite -(subnKC le_dm) addnA -mulSnr.
by apply: IHm q.+1 (n-d) _; apply: leq_trans (leq_subr d n) leq_nm.
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
