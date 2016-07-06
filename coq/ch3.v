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

elim: (n.+4) => // m IH m16.

move: n {-2}n (leqnn n). elim. by case.
move=> n IHn.

 do 11! [ case=>// ]; move=> m m_lt_n m11.
case: (boolP (m <= n)); first by move=> *; apply IHn.
rewrite -ltnNge=> H. have:= leq_trans H m_lt_n.
case: IHn m_lt_n m11.





Fixpoint fibo n :=
  match n with
  | 0 => 0
  | 1 => 1
  | (o.+1 as m).+1 => fibo o + fibo m
  end.

Lemma sum_odd_fibo n :
  foldr addn 0 [seq fibo x | x <- iota 0 n & odd x] =
  fibo (n.*2).
Proof.
rewrite -[RHS]addn0.
elim: n {-2}0 => // n IHn acc.
rewrite -addn1 iota_add filter_cat map_cat foldr_cat add0n /=.
case: (boolP (odd n)) => /=.
  rewrite IHn doubleD.




Lemma strong P :
 P 0 -> (forall n, (forall m, m < n -> P m) -> P n) -> forall n, P n.
Proof.
move=> base step n.
elim: n {-2}n (leqnn n).
  by case.
move=> m Pm n n_ltn_m. apply: step => w w_lt_n. apply: Pm.


  