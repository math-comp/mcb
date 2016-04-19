(* Version of string_of_pearls using pure Math Comp, by Cyril *)
From mathcomp Require Import all_ssreflect ssralg zmodp.

Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Import GRing.Theory.
Local Open Scope ring_scope.

Theorem Fermat_little_necklace p n : prime p -> p %| n ^ p - n.
Proof.
case: p => [|[|p']] //; set p := p'.+2 => p_prime.
pose monocolor : {set 'I_n ^ p} := [set [ffun=> c] |c : 'I_n].
suffices: p %| #|~: monocolor|.
  rewrite cardsCs setCK card_ffun ?card_imset ?card_ord //.
  by move=> c1 c2 /ffunP/(_ 0); rewrite !ffunE.
pose cycle (nl : 'I_n ^ p) d : 'I_n ^ p := [ffun i : 'Z_p => nl (i + d)].
have cycle_id nl d : d != 0 -> cycle nl d = nl -> forall m, cycle nl m = nl.
  move=> d_neq0 nld m; rewrite -[m](@mulrVK _ d); last first.
    by rewrite [_ \in _]prime_coprime // /dvdn modZp (val_eqE _ Zp0).
  apply/ffunP=> i; rewrite !ffunE -[m / d]natr_Zp mulr_natl.
  elim: (val _) => //= [|k IHk]; first by rewrite mulr0n addr0.
  by have /ffunP/(_ (i + d *+ k)) := nld; rewrite !ffunE mulrSr addrA => ->.
pose similar (nl1 nl2 : 'I_n ^ p) := [exists d : 'Z_p, nl1 == cycle nl2 d].
have eqp := @equivalence_partitionP _ similar (~: monocolor).
rewrite (@card_uniform_partition _ p _ _ _ \o eqp) ?dvdn_mull //; last first.
  move=> x y z; split.
    by apply/'exists_eqP; exists 0; apply/ffunP=> i; rewrite ffunE addr0.
  move=> /'exists_eqP [dxy ->]; apply/'exists_eqP/'exists_eqP => [] [d yz];
    eexists; apply/ffunP => i; have /ffunP := yz.
    by move=> /(_ (i - dxy)); rewrite !ffunE addrNK -addrA => ->.
  by move=> /(_ (i + dxy)); rewrite !ffunE -addrA => ->.
move=> _ /imsetP [/= nl nl_multi] ->.
have -> : [set y in ~: monocolor | similar nl y] = [set cycle nl d | d : 'Z_p].
  apply/setP => /= nl'; rewrite inE.
  apply/idP/imsetP => /= [/andP [nl'_multi /'exists_eqP /= [d ->]]|].
    by exists (- d) => //; apply/ffunP => /= i; rewrite !ffunE /= addrNK.
  move=> [d _] nl'E; have snl: similar nl nl'.
    rewrite nl'E; apply/'exists_eqP => /=.
    by exists (- d) => //; apply/ffunP => /= i; rewrite !ffunE /= addrNK.
  rewrite snl andbT; move: snl nl_multi => /'exists_eqP /= [d' ->].
  rewrite !inE; apply: contra => /imsetP [c _ ->].
  by apply/imsetP; exists c => //; apply/ffunP=> i; rewrite !ffunE.
rewrite card_imset ?card_ord //= => d1 d2 eq_nl.
apply: contraTeq nl_multi; rewrite inE negbK -subr_eq0 => neq_d.
apply/imsetP; exists (nl 0) => //; apply/ffunP => i; rewrite ffunE.
suff /(_ i)/ffunP/(_ 0): forall m, cycle nl m = nl by rewrite ffunE add0r.
apply/(cycle_id _ _ neq_d _)/ffunP => j; have /ffunP/(_ (j - d2)) := eq_nl.
by rewrite !ffunE addrNK addrAC -addrA.
Qed.
