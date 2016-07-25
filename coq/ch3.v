Require Import all_ssreflect.

Lemma stamps n : 12 <= n -> exists s4 s5, s4 * 4 + s5 * 5 = n.
Proof.
move: {-2}n (leqnn n).
elim: n {-2}n (leqnn n) =>[|n IHn]; first by case.
do 12! [ case=> // ].
case; first by exists 3, 0.
case; first by exists 2, 1.
case; first by exists 1, 2.
case; first by exists 0, 3.
move=> m'; set m := _.+1; move=> mn m11.
case: (IHn (m-4) _ isT) => [|s4 [s5 def_m4]].
  by rewrite leq_subLR (leq_trans mn) // addSnnS leq_addl.
by exists s4.+1, s5; rewrite mulSn -addnA def_m4 subnKC.
Qed.
