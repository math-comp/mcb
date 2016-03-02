(* Version of Euler proof of Fermat little Theorem pure Math Comp, by Cyril *)
From mathcomp Require Import all_ssreflect.

Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Theorem Fermat_little_euler p n : prime p -> p %| n ^ p - n.
Proof.
case: p => [|[|p']] //; set p := p'.+2 => p_prime.
rewrite -eqn_mod_dvd; last first.
  have [->|n_gt0] := posnP n; first by rewrite exp0n.
  by rewrite -{1}[n]expn1 leq_pexp2l.
apply/eqP; elim: n => [|n IHn]; first by rewrite exp0n // ?subn0.
rewrite -addn1 expnDn big_ord_recl big_ord_recr addnA
   ?binn ?bin0 ?exp1n ?mul1n ?muln1 ?subn0 ?subnn ?expn0 addnAC //=.
rewrite -modnDmr; set s := \sum__ _; suff /eqP -> : s == 0 %[mod p].
  by rewrite addn0 -modnDml IHn modnDml.
rewrite eqn_mod_dvd // subn0 dvdn_sum // => i _.
by rewrite dvdn_mulr // prime_dvd_bin //= /bump add1n ltnS ltn_ord.
Qed.
