From mathcomp Require Import all_ssreflect.

Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Section TheNecklaceProof.
(* The colors *)
Variable  a : finType.

Section Cycle.

Definition cycle  (l : seq a) m:= iter m (rot 1) l.

Lemma cycle_size l m: size (cycle l m) = size l.
Proof.
by elim: m => // m  IHm; rewrite size_rot. 
Qed.

Lemma cycle_tupleP n (t : n.-tuple a) m : size (cycle t m) == n.
Proof.
by elim: m; rewrite ?size_tuple //= => m  IHm; rewrite size_rot. Qed.
Canonical cycle_tuple n  m (t : n.-tuple a) := Tuple (cycle_tupleP t m).

Lemma cycle_0  l: cycle l 0 = l.
Proof. by []. Qed.

Lemma cycle_add l m p : cycle  (cycle l p ) m = cycle l (m + p).
Proof. by rewrite /cycle -iter_add. Qed.

Lemma cycle_inE p c (l : seq a): c \in l = (c \in (cycle l p)).
Proof. by elim:p => //= p Ip;rewrite mem_rot. Qed.


Lemma cycle_rot: forall p l, p <= (size l) -> cycle l p = rot p l.
Proof.
elim; first by  move=> l _;rewrite  rot0 cycle_0.
move=> n Hn l sl.
by rewrite -add1n -cycle_add rot_addn  ?Hn // ltnW.
Qed.

Lemma cycle_back (l:seq a): cycle l (size l) = l.
Proof. by rewrite cycle_rot ?rot_size. Qed.

Lemma cycle_multiple_back l n m: (cycle l n ) = l -> cycle l (m * n)  = l.
Proof.
by move=> Hn; elim: m =>// m Im; rewrite mulSn -cycle_add Im Hn.
Qed.

Lemma cycle_mod_length n (l:seq a): cycle l (n %% (size l))  = cycle l n .
Proof.
rewrite [in RHS](divn_eq  n (size l)) addnC -cycle_add.
by rewrite cycle_multiple_back // cycle_back.
Qed.

Lemma cycle_inv (l:seq a) n: n <= (size l) ->
                          cycle (cycle l n) ((size l ) - n)= l.
Proof.
by move=> nsl; rewrite cycle_add subnK // cycle_back.
Qed.

Lemma cycle_mod_add_length (l:seq a) n m :
     cycle  (cycle l n) m = cycle l ((m + n) %% (size l)).
Proof. by rewrite cycle_add cycle_mod_length. Qed.

Lemma cycle_gcd l m n : (cycle l m ) = l -> cycle l n  = l ->
                             cycle l (gcdn m n)  = l.
Proof.
move=> cycm; case:n =>[| n Hn]; first by rewrite gcdn0.
case:(@Bezoutr m n.+1)=>// q Hq /dvdnP [p Hp].
rewrite -[l in LHS](cycle_multiple_back q cycm) //.
by rewrite cycle_add Hp cycle_multiple_back.
Qed.

Lemma cycle_nil n: cycle [::] n = [::].
Proof. by elim:n =>//= n ->. Qed.

Lemma cycle_l_nil l n: cycle l n == [::] = ( l == [::]).
Proof.
elim: n => //= n Hn.
have {1}<- : rot 1 [::] = [::] by [].
apply/eqP/idP.
  by move/rot_inj; rewrite -Hn => ->.
by move/eqP ->; rewrite cycle_nil.
Qed.

End Cycle.

(* A necklace is of size n *)
Definition necklace n  : finType := [finType of n.-tuple a].

Lemma card_necklace n  : #| necklace n | = #|a| ^ n.
Proof. exact: card_tuple. Qed.

Definition mono n  : {set necklace n } :=
  [set t : necklace n  |  if val t is x :: xs then all (pred1 x) xs else true ].
Definition  mono_coloured {n} (l: necklace n) := l \in (mono n).

Lemma mono0 (l: (necklace 0)):(mono_coloured l).
Proof.
by case:l=> [[h| x l //]]; rewrite /mono_coloured inE.
Qed.

Definition multi n  := setC (mono n ).
Definition multi_coloured {n} (l: necklace n) := l \in (multi n).
Definition colors (l : seq a ):= #|l|.


Definition set_of_color (l : seq a ):=[set x | x \in l].

Lemma mono_multi_in n (l: necklace n ): l \in (multi n )= (l \notin (mono n)).
Proof. by rewrite -in_setC. Qed.

Lemma multiNmono  n (l: necklace n ): multi_coloured l = ~~(mono_coloured l).
Proof.
by rewrite /multi_coloured /mono_coloured mono_multi_in. 
Qed.


Lemma monoP : forall n  (l: necklace n.+1 ),
reflect (set_of_color l = [set (tnth l ord0)]) (l \in (mono n.+1 )).
Proof.
move=> n l; rewrite /set_of_color !inE.
case :l=>[[//|c] l size_l]; rewrite (tnth_nth  c) /= set_cons.
apply: (iffP idP ).
  move/all_pred1P=>defl; apply/setP => x; rewrite !inE defl.
  case:(boolP (x \in _)); rewrite ?orbF ?orbT //.
  by move/nseqP=>[-> _]; rewrite eqxx.
move/setP=>Hc; apply/allP=> x  hx.
by move:(Hc x); rewrite !inE hx orbT.
Qed.

Lemma monoE : forall n  (l: necklace n.+1 ),
(set_of_color l == [set (tnth l ord0)]) = (l \in (mono n.+1 )).
Proof.
by move=> n l; apply/idP/idP;[move=>/eqP h| move=> h; apply /eqP]; apply/monoP.
Qed.

Lemma mono_card1 n  (l: necklace n.+1 ):
     l \in (mono n.+1 )-> #|set_of_color l| = 1.
Proof. by move/monoP ->; rewrite cards1. Qed.

Lemma card1_mono n  (l: necklace n.+1 ):
     #|set_of_color l| = 1 -> l \in (mono n.+1 ).
Proof.
move=>Hcard;apply/monoP.
apply/eqP; rewrite eq_sym eqEcard Hcard cards1 ltn0Sn andbT.
apply/subsetP => x.
by rewrite !inE /set_of_color =>/eqP ->; rewrite mem_tnth. 
Qed.

Lemma mono_cst {n}  (l: necklace n.+1 ):
reflect (forall i , i \in l -> i = (tnth l ord0)) (l \in (mono n.+1)).
Proof.
case:l; move=>[//|c l] sl; rewrite /mono inE /=.
apply: (iffP allP ).
  move=> allpl i.
rewrite !inE (tnth_nth c) /=.
case:(boolP (i \in _)); rewrite ?orbT ?orbF; last by move=> _ /eqP ->.
by move/(allpl i); move => /eqP.
  move=>  H i.
move:(H i).
rewrite !inE (tnth_nth c) /=.
by case:(i \in _); rewrite ?orbT ?orbF //= => ->.
Qed.


Lemma mono_cstl n  (l: necklace n.+1 ): l \in (mono n.+1)-> 
                           forall i , i \in l -> i =(tnth l ord0).
Proof.
case:l; move=>[//|c l] sl; rewrite /mono inE /=.
move/allP =>allpl i; rewrite !inE (tnth_nth c) /=.
case:(boolP (i \in _)); rewrite ?orbT ?orbF; last by move=> _ /eqP ->.
by move/(allpl i); move => /eqP.
Qed.

Lemma card_mono n : #| mono n.+1  | = #| a |.
Proof.
have Hf c : size (nseq n.+1 c) == n.+1 by rewrite size_nseq.
pose f (c : a) := Tuple (Hf _ c).
have inj_f : injective f.
   by move=> x y => [[->]].
rewrite -[#|a|](card_imset _ inj_f). 
apply: eq_card => [[[//|x xs] size_x]]; rewrite !inE /=.
apply/all_pred1P/idP=> [xs_all_x |].
  apply/imsetP; exists x => //=; apply: val_inj; rewrite /= xs_all_x.
  by move/eqP: size_x => [[->]].
by case/imsetP=> [y y_a [-> ->]]; rewrite size_nseq.
Qed.



Lemma card_multi n  : #| multi n.+1  | = #| a | ^ n.+1 - #| a |.
Proof.
rewrite -card_necklace -[#|a|](card_mono n).
by rewrite -(setCK (mono n.+1 )) -cardsCs.
Qed.

Lemma cycle_color p l: set_of_color (cycle  l p) = set_of_color l.
Proof.
rewrite /set_of_color.
apply/setP=>x; rewrite !inE /cycle.
by elim:p=>//= p Ip; rewrite mem_rot Ip.
Qed.


Lemma cycle_mono {n} (l: necklace n ) p : mono_coloured l =
                                (mono_coloured [tuple of (cycle l p)]).
Proof.
case:n l=>[l|n l]; first by rewrite !mono0.
apply/mono_cst/mono_cst=>H c.
  rewrite -cycle_inE (H (tnth _ _)); first exact: (H c).
  by rewrite (cycle_inE  p) mem_tnth.
rewrite (cycle_inE  p)(H (tnth _ _)); first exact:(H c).
by rewrite -cycle_inE mem_tnth.
Qed.

Lemma cycle_multi n p (l: necklace n ):
        (multi_coloured [tuple of (cycle l p)])= multi_coloured l.
Proof.
by move:(cycle_mono l p); rewrite !multiNmono=> ->.
Qed.

(* Similarity and partitions *)

Definition similar l1 l2:= exists n , l2 = cycle l1 n.

Definition similarb l1 l2:= if (l1 == [::]) then (l2 == [::]) else
                          [exists n :'I_(size l1), l2 == cycle  l1 n].

Notation "x === y":= (similarb x y)(at level 70, no associativity).


Lemma sim_nill l: (similar l [::]) <-> l = [::].
Proof.
split.
  by case=> n /eqP; rewrite eq_sym cycle_l_nil=>/eqP.
by move->; exists 0.
Qed.

Lemma sim_nilr l: (similar  [::] l) <-> l = [::].
Proof.
split; last by move->; exists 0.
case=>n Hl; apply/eqP; rewrite -(cycle_l_nil _ n).
by rewrite Hl cycle_add cycle_nil.
Qed.

Lemma sim_card l1 l2 : similar l1 l2 ->  size l1 = size l2.
Proof. by case=> n ->; rewrite cycle_size. Qed.

Lemma sim_soc l1 l2: similar l1 l2 -> set_of_color l1  = set_of_color l2.
Proof. by case=> n ->; rewrite cycle_color. Qed.

Lemma similarP  l1 l2: reflect (similar l1 l2) (similarb l1 l2).
Proof.
case:l1; case :l2.
  + rewrite /similar  /similarb eqxx; apply ReflectT.
    by exists 0.
  + move => a0 l; rewrite /similarb ; apply/ReflectF.
    by case=> x; rewrite cycle_nil.
  + move => a0 l; rewrite /similarb /similar; apply (iffP idP).
      move/existsP; case=> x /eqP hx.
      by move:(cycle_size  (a0 :: l) x); rewrite -hx.
    move=>[n hn].
    by move:(cycle_size (a0 :: l) n); rewrite -hn.
move=> a0 l0 a1 l1; apply:(iffP idP).
  move/existsP; case=> x /eqP hx.
  by exists x; rewrite hx.
move=>[x hx]; apply/existsP.
exists (Ordinal (ltn_pmod x (ltn0Sn (size  l1))))=>/=.
have <-: (size (a1 :: l1) = (size l1).+1) by done.
by rewrite hx cycle_mod_length.
Qed.

Lemma sim_refl l : (similar l l).
Proof. by exists 0. Qed.

Lemma simb_refl l : (l === l ).
Proof.
by apply/similarP; apply sim_refl.
Qed.

Lemma sim_sym l1 l2 : (similar l1 l2)-> (similar l2 l1).
Proof.
case:l1=> [Hl1| x l ].
  by case:(sim_nilr l2)=>/(_ Hl1) -> _; exists 0.
set l1 := (x :: l).
case=> n ->; rewrite (divn_eq n (size l1)).
rewrite (addnC _ (_ %% _)) -cycle_add cycle_multiple_back ?cycle_back //.
exists ((size l1) - n%%(size l1)); rewrite cycle_inv //.
by rewrite ltnW // ltn_mod /l1.
Qed.

Lemma simb_symi l1 l2 : (similarb l1 l2)-> (similarb l2 l1).
Proof.
by move/similarP=> simb;  apply/similarP; apply:sim_sym.
Qed.

Lemma simb_sym l1 l2 : (similarb l1 l2) = (similarb l2 l1).
Proof.
case:(boolP (similarb l1 l2)).
  by move/simb_symi ->.
by move=>h; rewrite (contraNF (@simb_symi l2 l1)).
Qed.

Lemma sim_trans l1 l2 l3: (similar l1 l2) /\ (similar l2 l3) ->
                          (similar l1 l3).
Proof.
case;case => n1 H1; case=> n2 H2; exists (n2+n1).
by rewrite -cycle_add -H1 -H2.
Qed.

Lemma simb_trans l1 l2 l3: (l1 === l2) -> (l1 === l3) = (l2 === l3).
Proof.
case=>/similarP sim12.
case:(boolP(l2 === l3)).
  by move/similarP=>sim23; apply/similarP; apply:(@sim_trans _ l2).
have ht: (l1 === l3)->(l2 === l3).
  move/similarP=> sim13; apply/similarP; apply: (@sim_trans _ l1); split=>//.
  by apply: sim_sym.
by move =>h; rewrite (contraNF ht).
Qed.


(* to reconsider...*)
(* Definition associates x := [set y:((size x).-tuple a)| similarb x y].*)

Definition associates p (l:necklace p): {set necklace p} :=
  [set l' : necklace p | l' === l].


Definition associates' p  (l:necklace p):(seq (necklace p) ):=
[seq [tuple of (cycle l (val n) )] | n in ordinal_finType p].

Lemma mono_nseqE {n}  (l: necklace n.+1 ): 
reflect (val l = (nseq n.+1 (tnth l ord0)))(mono_coloured l).
Proof.
case:l; case=>//= x l sl1.
have sl: size l = n by move:sl1; rewrite eqSS=>/eqP ->.
rewrite (tnth_nth x) /= /mono_coloured inE /= -sl.
apply:(iffP idP).
  by move=> /all_pred1P {1}->.
move/eqP; rewrite eqseq_cons=>/andP [ _ /eqP hnseq].
by apply/all_pred1P.
Qed.

Lemma mono_cycle1 {n}  (l: (necklace n)):  (mono_coloured l) ->  (cycle l 1= l).
Proof.
case: n l=>[| n l].
  by case=> l h _ /=;  case: l h.
move/mono_nseqE=>->; rewrite /= /rot /=.
rewrite drop0 take0 cat_nseq.
suff ->: forall T (x:T) m, ncons m x [:: x] = x :: nseq m x by [].
by move=> T x; elim=>//= m ->.
Qed.



Variable c: a.


Lemma nth_cycle n l: nth c (cycle l 1) (n %% size l) = nth c l (n.+1 %% size l).
elim:l=>[| a0 l Ihl].
  by rewrite cycle_nil nth_nil.
have -> : cycle  (a0::l) 1 = l++[::a0].
  by case:l {Ihl}=>[| a1 l]; rewrite /= /rot.
rewrite nth_cat. 
have : n %% size (a0 :: l) < (size (a0:: l)) by rewrite ltn_mod.
rewrite /= ltnS leq_eqVlt; case/orP.
  move/eqP => e; rewrite e ltnn subnn /=.
  suff -> :(n.+1 %% (size l).+1)= 0 by [].
  rewrite -addn1.
  by rewrite -modnDml e addn1 modnn.
move => nlt; rewrite nlt.
suff -> : (n.+1 %% (size l).+1)= (n %% (size l).+1).+1. 
  by rewrite -nth_behead.
rewrite -addn1  -(addn1 (n %% (size l).+1)).
by rewrite -modnDml modn_small // addn1.
Qed.




Lemma nth_cyclen m n l : nth  c (cycle l n ) (m%%(size l)) = nth c  l ((n + m)%% (size l)).
Proof.
elim:  n m l.
 by move =>m l; rewrite cycle_0 add0n.
move=> n Ihn m l.
rewrite -{1}addn1  -cycle_add.
move:(Ihn m (cycle l 1)).
rewrite cycle_size=>->.
rewrite addSn.
case:l.
  by rewrite cycle_nil !nth_nil.
move=> a1 l.
by rewrite nth_cycle.
Qed.


Lemma head_cycle: forall l m, head c (cycle l m ) = (nth c l (m%%(size l))).
Proof.
move=> l m.
by rewrite -nth0 -(mod0n (size l)) (nth_cyclen 0 m) addn0.
Qed.



Lemma cycle1_mono {n}  (l: (necklace n)): (cycle l 1 = l)->(mono_coloured l).
Proof.
case: n l=>[l _| n l hl]; first by exact:mono0.
have H m :cycle l m = l.
  by rewrite -(muln1 m) cycle_multiple_back.
apply/mono_cst=>c0 /tnthP.
move=>[[i hi]]/= ->.
rewrite (tnth_nth (tnth l ord0)) /=.
move:(head_cycle l i).
rewrite (H i) size_tuple modn_small // -nth0.
rewrite (set_nth_default c ); last by rewrite size_tuple.
by move <-; rewrite (tnth_nth c).
Qed.


Lemma mono_cycle1P {n}  (l: (necklace n)): 
        reflect (cycle l 1 = l)  (mono_coloured l).
Proof.
case:n l=>[[[|x l] ls]|n l]=>//.
    by rewrite mono0/= /rot; apply:ReflectT.
apply(iffP idP).
  by apply:mono_cycle1.
by apply: cycle1_mono.
Qed.

Variable p:nat.
Hypothesis p_prime: prime p.

Lemma aux3: forall (x y : 'I_p) (l: necklace p), multi_coloured l -> cycle l x = cycle l y  -> x = y.
Proof.
move=> x y l ml; wlog lt_xy: y x / x <= y.
  move=> hwlog.
  case/orP: (leq_total x y).
    by apply: hwlog.
  move=>hxy /eqP; rewrite eq_sym=> /eqP hcyc.
  by apply:sym_eq; apply:hwlog.
move => hcyc; move:lt_xy; rewrite leq_eqVlt.
case/orP=> hxy; first by apply/eqP.
pose d := y - x.
have dpos: 0 < d by rewrite /d subn_gt0.
have dltp: d < p by apply:(leq_ltn_trans(leq_subr x y)).
have dDx: (d + x = y) by rewrite /d subnK // ltnW.
pose l' := [tuple of (cycle l x)].
have cycl': (cycle l' d)= l'.
  by rewrite /l' cycle_add dDx -hcyc.
move:(cycle_back  l'); rewrite size_tuple.
move/(cycle_gcd  _ _ _  cycl').
have ->:  gcdn d p = 1.
  rewrite gcdnC.
  apply/eqP; move:(prime_coprime d  p_prime); rewrite /coprime => ->.
  by rewrite gtnNdvd.
move/mono_cycle1P.
rewrite /l' -(cycle_mono l x) => monol.
by move: ml; rewrite multiNmono monol.
Qed.

Theorem cyclen_inj: forall (l: necklace p), multi_coloured l -> 
     injective (fun n : 'I_p => (cycle l n)).
Proof.
move=> l ml  x y.
by apply: aux3.
Qed.



Lemma aux4: forall (l : necklace p) l' n, multi_coloured l -> l' = (cycle  l n)->
exists (k:'I_p) , l' = cycle l k.
Proof.
move=> l l' n ml.
rewrite -cycle_mod_length  size_tuple => ->.
 by exists (Ordinal (ltn_pmod  n(prime_gt0 p_prime))).
Qed.



Theorem card_assoc: forall (l : necklace p), multi_coloured l -> 
                     #|(associates _ l)| = p.
Proof.
move=> l ml.
rewrite -[in RHS](card_ord p).
rewrite /associates.
move:(@card_codom _  _(fun n:'I_p => [tuple of (cycle l n)]))=> <-; first last.
move => x y H.
rewrite (cyclen_inj l ml x y)//.
(* ???*)
by rewrite (f_equal (fun t => tval t) H).
apply/eq_card=>x.
rewrite !inE.



apply/idP/idP.
rewrite simb_sym.
move/similarP=>[n hn].
apply/codomP.

exists (Ordinal (ltn_pmod n (prime_gt0 p_prime))).
rewrite /=.
apply/val_inj=> /=.
by rewrite -cycle_mod_length  size_tuple in hn.
move/codomP => [n hn].
rewrite simb_sym; apply/similarP.
exists n.
by rewrite hn //=.
Qed.


Notation n := #|a|.










Theorem Fermat_little_th: p %| n^p - n.
Proof.
have ppos: p = p.-1.+1 by rewrite prednK // prime_gt0. 
rewrite ppos -(card_multi p.-1) -ppos.
pose part:=(@equivalence_partition (necklace p) similarb (multi p)).
suff->: #|multi p|=#|part| * p by rewrite dvdn_mull.
have: partition part  (multi p).
  apply/equivalence_partitionP.
  move=> x y z => hx hy hz; split.
    by rewrite simb_refl .
  by move=> ?; rewrite (simb_trans _ y).
apply:card_uniform_partition.

move=>x;
rewrite /part/equivalence_partition.
move/imsetP.
 case=> l lm ex.
rewrite ex.

move:(card_assoc l lm).
rewrite /associates=>  h.
rewrite -[in RHS]h.
apply eq_card => y.
rewrite inE [in RHS]inE.
rewrite simb_sym.
apply:andb_idl.
rewrite simb_sym; move/similarP.
case.
move=> n hy.
move: (cycle_multi p n l).

have ->: multi_coloured [tuple of cycle l n]= multi_coloured y.
have: cycle l n = val [tuple of cycle l n].
 by done.
rewrite -{1}hy.
move/val_inj.
by move->.
by rewrite /multi_coloured lm.

Qed.



End TheNecklaceProof.


