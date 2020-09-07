From mathcomp Require Import all_ssreflect.


Check 3 : nat.

Section CheckNat.

Variable n : nat.
Check n + n : nat.

Fail Check n + n : bool.

Check muln0 n : n * 0 = 0.

End CheckNat.

Check Prop.

Lemma stamps n : 12 <= n -> exists s4 s5, s4 * 4 + s5 * 5 = n.
Proof.
have [m leq_mn] := ubnPgeq n; elim: n => // n IHn in m leq_mn *; first by case: n => [|n] in IHn *.
do 12! [ case: m => //= m in leq_mn * ] => _.
case: m => [|m] in leq_mn *; first by exists 3, 0.
case: m => [|m] in leq_mn *; first by exists 2, 1.
case: m => [|m] in leq_mn *; first by exists 1, 2.
case: m => [|m] in leq_mn *; first by exists 0, 3.
case: (IHn ((16+m) - 4) _ isT) => [|s4 [s5 def_m4]]. 
  by rewrite leq_subLR (leq_trans leq_mn) // addSnnS leq_addl.
by exists s4.+1, s5; rewrite mulSn -addnA def_m4 subnKC.
Qed.
