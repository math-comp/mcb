(* Version of string_of_pearls using pure Math Comp, by Cyril *)

From mathcomp Require Import all_ssreflect ssralg perm zmodp cyclic fingroup.

Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Import GRing.Theory.

Section TheNecklaceProof.
Section Necklace.
Variable (color : finType) (p' : nat).
Let p := p'.+2.

Local Notation necklace := (color ^ p)%type.
Definition monocolor : {set necklace} := [set [ffun=> c] |c : color].

Lemma card_monocolor : #|monocolor| = #|color|.
Proof. by rewrite card_imset => // c1 c2 /ffunP/(_ ord0); rewrite !ffunE. Qed.

Definition cycle (nl : necklace) d := [ffun i : 'Z_p => nl (i + d)%R].

Lemma cycle_eq_mul nl m d : cycle nl d = nl -> cycle nl (m * d)%R = nl.
Proof.
move=> nld; apply/ffunP=> i; rewrite !ffunE.
suff : forall m : nat, nl (i + (d *+ m))%R = nl i.
  move=> /(_ (val m)) <-; congr (nl (_ + _)%R); apply: val_inj => /=.
  by rewrite Zp_mulrn /= mulnC.
elim => //= [|n IHn]; first by rewrite mulr0n addr0.
have /ffunP /(_ (i + d *+ n)%R) := nld; rewrite !ffunE.
by rewrite mulrSr addrA => ->.
Qed.

Definition similar nl1 nl2 := [exists d : 'Z_p, nl1 == cycle nl2 d].

Lemma similar_equiv : equivalence_rel similar.
Proof.
move=> x y z; split.
  by apply/'exists_eqP; exists 0%R; apply/ffunP=> i; rewrite ffunE addr0.
move=> /'exists_eqP /= [dxy ->].
apply/'exists_eqP/'exists_eqP => /= [[d yz]|[d yz]].
  eexists; apply/ffunP => i; have /ffunP /(_ (i - dxy)%R) := yz.
  by rewrite !ffunE addrNK -addrA => ->.
eexists; apply/ffunP => i; have /ffunP /(_ (i + dxy)%R) := yz.
by rewrite !ffunE -addrA => ->.
Qed.

Lemma similar_multicolor nl1 nl2 : similar nl1 nl2 ->
  nl1 \in ~: monocolor -> nl2 \in ~: monocolor.
Proof.
move=> /'exists_eqP /= [d ->]; rewrite !inE; apply: contra => /imsetP [c _ ->].
by apply/imsetP; exists c => //; apply/ffunP=> i; rewrite !ffunE.
Qed.

Lemma pdvd_multicolor : prime p -> p %| #|~: monocolor|.
Proof.
move=> p_prime; apply/dvdnP.
have := @equivalence_partitionP _ _
   (~: monocolor) (fun x y z _ _ _ => similar_equiv x y z).
move=> /(@card_uniform_partition _ p) -> /=; first by eexists.
move=> _ /imsetP /= [nl nl_multi] ->.
have -> : [set y in ~: monocolor | similar nl y] = [set cycle nl d | d : 'Z_p].
  apply/setP => /= nl'; rewrite inE.
  apply/idP/imsetP => /= [/andP [nl'_multi /'exists_eqP /= [d ->]]|].
      by exists (-d)%R => //; apply/ffunP => /= i; rewrite !ffunE /= addrNK.
  move=> [d _] nl'E; suff snl: similar nl nl'.
    by rewrite snl (similar_multicolor snl).
  rewrite nl'E; apply/'exists_eqP => /=.
  by exists (-d)%R => //; apply/ffunP => /= i; rewrite !ffunE /= addrNK.
rewrite card_imset ?card_ord //= => d1 d2 eq_nl.
apply: contraTeq nl_multi => neq_d; rewrite inE negbK.
suff eq_all_nl m : cycle nl m = nl.
  apply/imsetP; exists (nl ord0) => //; apply/ffunP => i; rewrite ffunE.
  by have /(_ i) /ffunP /(_ ord0) := eq_all_nl; rewrite !ffunE add0r.
rewrite -[m]mulr1 -[1%R](@Zp_mulVz _ (d2 - d1)%R); last first.
  rewrite prime_coprime //; apply/negP => /dvdn_leq.
  rewrite lt0n ltnNge leq_ord.
  have : (d2 - d1 != 0)%R by rewrite subr_eq0 eq_sym.
  by rewrite -val_eqE /= => -> /(_ isT).
rewrite mulrA; apply: cycle_eq_mul => //; apply/ffunP => i; rewrite ffunE.
by have /ffunP /(_ (i - d1)%R) := eq_nl; rewrite !ffunE addrNK addrAC -addrA.
Qed.

End Necklace.

Theorem Fermat_little_necklace p n : prime p -> p %| n ^ p - n.
Proof.
move=> p_prime; have p_gt0 := prime_gt0 p_prime.
have pB1_gt0 : 0 < p.-1 by rewrite -subn1 subn_gt0 prime_gt1.
have := @pdvd_multicolor [finType of 'I_n] p.-2.
by rewrite cardsCs setCK card_ffun card_monocolor !card_ord !prednK //; apply.
Qed.

End TheNecklaceProof.

Section TotientProof.

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

End TotientProof.
