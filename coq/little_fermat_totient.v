From mathcomp Require Import all_ssreflect cyclic.

Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Theorem Fermat_little_th p n : prime p -> p %| n ^ p - n.
Proof.
move=> p_prime; have p_gt0 := prime_gt0 p_prime.
have [p_dvdn|pNdvdn] := boolP (p %| n); first by rewrite dvdn_sub // dvdn_exp.
have [->|n_gt0] := posnP n; first by rewrite exp0n // subnn.
rewrite -eqn_mod_dvd; last by rewrite -{1}[n]expn1 leq_pexp2l.
have {1}->: p = (totient (p ^ 1)).+1 by rewrite totient_pfactor ?muln1 ?prednK.
rewrite expnS -modnMmr Euler_exp_totient 1?coprime_sym ?prime_coprime //.
by rewrite ?[1 %% _]modn_small ?prime_gt1 ?muln1.
Qed.
