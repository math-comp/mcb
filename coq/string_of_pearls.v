From mathcomp Require Import all_ssreflect.

Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Section TheNecklaceProof.
(* The colors *)
Variable  a : finType.

Section Cycle.


Definition cycle   (l: seq a)  m := (rot (m %% (size l)) l).

Lemma cycle_0  l : cycle l 0 = l.
Proof. by rewrite /cycle mod0n rot0. Qed.

Lemma cycle_size  l m: size (cycle l m) = (size l).
Proof. by rewrite size_rot. Qed.


Lemma cycle_add l  m p : cycle  (cycle l p ) m = cycle l (m + p).
Proof.
case:l=>[//| c l].
have leq_modr n:   n %% (size (c::l)) <= (size (c::l)) by rewrite ltnW // ltn_pmod.
rewrite /cycle rot_add_mod // size_rot //.
set s:= size(c::l).
case:(boolP(m %% s + p %% s <= s))=>/=.
  rewrite leq_eqVlt =>/orP [].
    rewrite -modnDm; move/eqP ->.
    by rewrite  modnn rot0 rot_size.
  by move=> gts; rewrite -modnDm [in RHS]modn_small.
rewrite -ltnNge=> lts.
suff ->: (m %% s + p %% s - s) =  ((m + p) %% s) by done.
rewrite -modnDm.
rewrite -(@modn_small(_ - _) s) /s; 
    first by  rewrite -modnDr subnK // (ltn_trans(ltnSn (size l))).
rewrite -(ltn_add2r s) subnK; last by rewrite (ltn_trans (ltnSn (size l))). 
by rewrite (@ltn_trans (s + p %% s)) // ?ltn_add2r ?ltn_add2l  ltn_pmod.
Qed.

Lemma cycle_mod_length l  m : cycle l (m %% (size l))  = cycle l m.
Proof. by rewrite /cycle modn_mod. Qed.

Lemma cycle_mod_add_length l  n m :
     cycle  (cycle l n) m = cycle l ((m + n) %% (size l)).
Proof. by rewrite cycle_add cycle_mod_length. Qed.

Lemma cycle_back l: cycle l (size l) = l.
Proof. by rewrite -cycle_mod_length modnn cycle_0. Qed.

Lemma cycle_multiple_back l n m: (cycle l n ) = l -> cycle l (m * n)  = l.
Proof.
move=> Hn; elim: m=>[| m Im]; first by rewrite mul0n cycle_0.
by rewrite mulSn -cycle_add Im Hn.
Qed.

Lemma cycle_inv  l  n: n <=  (size l)-> cycle (cycle l n) ((size l) - n)= l.
Proof.
by move=> nsl; rewrite cycle_add subnK // cycle_back.
Qed.

Lemma cycle_gcd  l m n : (cycle l m ) = l -> cycle l n  = l ->
                             cycle l (gcdn m n)  = l.
Proof.
move=> cycm; case:n =>[| n Hn]; first by rewrite gcdn0.
case:(@Bezoutr m n.+1)=>// q Hq /dvdnP [r Hr].
rewrite -[l in LHS](cycle_multiple_back q cycm) //.
by rewrite cycle_add Hr cycle_multiple_back.
Qed.

End Cycle.

(* A necklace is of size n *)
Definition necklace n  : finType := [finType of n.-tuple a].

Lemma card_necklace n  : #| necklace n | = #|a| ^ n.
Proof. exact: card_tuple. Qed.

Definition monocolor  n  : {set necklace n } :=
  [set t : (necklace n) |[exists c, [forall i,  tnth t  i == c]]].

Definition multicolor   n  := setC (monocolor n ).

Lemma card_mono n : #|monocolor  n.+1| = #|a|.
Proof.
have Hf c : size (nseq n.+1 c) == n.+1 by rewrite size_nseq.
pose f (c : a) := Tuple (Hf _ c).
have nseq_tupleE i c:  tnth (Tuple (Hf a c)) i = c.
  by rewrite  (tnth_nth c) nth_nseq; case:(_<_).
have inj_f : injective f by move=> x y => [[->]].
rewrite -[#|a|](card_imset _ inj_f). 
apply: eq_card => [[[//|x xs] size_x]]; rewrite !inE. 
apply/'exists_'forall_eqP/imsetP => /= [[c t_c]|[c hc ->]];exists c =>//.
  by apply/eq_from_tnth => i; rewrite t_c nseq_tupleE.
by move=> i;rewrite nseq_tupleE.
Qed.

Lemma card_multi n  : #| multicolor  n.+1  | = #| a | ^ n.+1 - #| a |.
Proof.
rewrite -card_necklace -[#|a|](card_mono n).
by rewrite -(setCK (monocolor n.+1 )) -cardsCs.
Qed.


(* Similarity and partitions *)

Lemma cycle_tupleP n (t : n.-tuple a) m : size (cycle  t m) == n.
Proof. by rewrite size_rot size_tuple. Qed.
Canonical cycle_tuple  n  m (t : n.-tuple a) := Tuple (cycle_tupleP t m).
Definition similar p (l1 l2:p.-tuple a)  := if p == 0 then true else [exists n :'I_p, l2 == [tuple of cycle  l1 n]].

Notation "x === y":= (similar x y)(at level 70, no associativity).

Lemma cycle_color p (l:p.-tuple a) n: #|(cycle  l n)| = #|l|.
Proof. by apply/eq_card=>x; rewrite mem_rot. Qed.

Lemma sim_card p (l1 l2:p.-tuple a)  : similar l1 l2 ->  #|l1| = #|l2|.
Proof.
rewrite /similar; case: p l1 l2=>[|n]l1 l2 //=.
  by move=> _; rewrite tuple0 card0 tuple0 card0.
by move/existsP=>[m /eqP ->]; rewrite cycle_color.
Qed.

Lemma sim_refl p (l:p.-tuple a) : (similar l l).
Proof. 
case: p l =>[|n]l; rewrite ?tuple0 //=.
apply/existsP; exists ord0.
by apply/eqP /val_inj;rewrite /= cycle_0. 
Qed.

Lemma sim_sym_trans  p (x y z :p.-tuple a):
  similar x y -> similar x z = similar y z.
Proof.
case:p x y z =>[|p]x y z //.
move=>/existsP. move=> /= [dxy /eqP ->].
apply/'exists_eqP/'exists_eqP=>/= [[dzy  zy]|[dzx  zx]];last first.
  exists (Ordinal (ltn_pmod (dzx + dxy) (ltn0Sn p))).
  by rewrite zx; apply/val_inj; rewrite /= cycle_add -cycle_mod_length size_tuple.
case:(boolP (dxy <= dzy))=> hd.
  pose h:= (leq_ltn_trans ( leq_subr dxy dzy )(ltn_ord dzy)).
  by exists (Ordinal h); rewrite zy; apply/val_inj;  rewrite /= cycle_add subnK.
rewrite -ltnNge in hd.
have hdp := (leq_trans (ltnW (ltn_ord dxy))(leq_addr dzy _)).
have h : (size x) + dzy - dxy < p.+1.
by rewrite size_tuple -(ltn_add2r dxy) subnK ?ltn_add2l.
exists (Ordinal h); rewrite zy; apply/val_inj; rewrite /=  cycle_add subnK.
  by rewrite  addnC -cycle_add cycle_back.
by rewrite size_tuple.
Qed.

Lemma sim_sym p (l1 l2:p.-tuple a) : (similar l1 l2) = (similar l2 l1).
Proof.
case: (boolP (similar l1 l2)) => [|h].
  by move/(sim_sym_trans l1); rewrite sim_refl.
apply: sym_eq; apply:(contraNF _ h).
by move/(sim_sym_trans l2); rewrite sim_refl.
Qed.

Lemma sim_trans p (l1 l2 l3:p.-tuple a):
 (similar l1 l2) /\ (similar l2 l3) -> (similar l1 l3).
Proof.
by move=>[/sim_sym_trans h1 h23]; rewrite (h1 l3) h23.
Qed.

Definition associates p (l:necklace p): {set necklace p} :=
  [set l' : necklace p | l' === l].



Lemma mono_cycle p (l: (necklace p)) m: (l \in (monocolor p))->(cycle l m = l).
Proof.
move=>monol.
suff : (cycle l 1 = l).
  by rewrite -[m]muln1=> ?; rewrite  cycle_multiple_back.
case:p l monol =>[l | p l ]; rewrite /monocolor /= ?tuple0 //.
rewrite inE=>/existsP [c /forallP  hc].
rewrite /cycle  size_tuple.
case:p l hc =>[l| p l] hc.
  by rewrite modnn /rot  /= drop0  take0 cats0.
apply/ sym_eq; apply/(@eq_from_nth _ c); first by rewrite size_rot.
move=> i hi. 
rewrite size_tuple in hi.
move/eqP: (hc (Ordinal hi)); rewrite (tnth_nth c) modn_small //= => ->.
rewrite /rot nth_cat size_drop size_tuple subn1 /=.
case:(boolP( i < p.+1)).
  rewrite nth_drop -(ltn_add2l 1) [x in _ < x]addnC addn1 // => hi1.
  by move:(hc (Ordinal hi1))=> /eqP  {1}<-;rewrite (tnth_nth c).
rewrite -leqNgt => hpi.
have hip: i <= p.+1 by rewrite -ltnS.
have : (p.+1 == i) by rewrite eqn_leq hip hpi.
move/eqP =>hpei; rewrite [x in _ - x]hpei subnn.
case:l hc; case => // x l hl.
by rewrite nth_take //; move/(_ ord0) /eqP.
Qed.

Lemma nth_cycle c n l: nth c (cycle l 1) (n %% size l) = nth c l (n.+1 %% size l).
elim:l=>[| a0 l Ihl].
  by rewrite  /=   nth_nil.
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


Lemma nth_cyclen c m n l : nth c  (cycle l n ) (m%%(size l)) = nth c  l ((n + m)%% (size l)).
Proof.
elim:  n m l.
 by move =>m l; rewrite cycle_0 add0n.
move=> n Ihn m l; rewrite -{1}addn1  -cycle_add.
move:(Ihn m (cycle l 1)); rewrite cycle_size=>->.
rewrite addSn; case:l; first by rewrite /=  !nth_nil.
by move=> a1 l; rewrite nth_cycle.
Qed.

Lemma cycle1_mono n (l: (necklace n.+1)) : (cycle l 1 = l)-> (l \in monocolor n.+1).
Proof.
case:n l =>//=.
  move=> l _; case:l; case=>//=.
  move=> c; case=>//= hi; rewrite inE.
  by apply/existsP; exists c; apply/forallP=> /=  [[[|x] hx]].
move=> n l cycle1.
have cyclem  m :cycle l m = l.
  by rewrite -(muln1 m) cycle_multiple_back.
rewrite /monocolor  inE.
pose c:= tnth l ord0.
apply/existsP; exists c; apply/forallP => i.
rewrite (tnth_nth c) -(@modn_small i n.+2) //.
rewrite -(cyclem (n.+2 -i))  -[X in _ %% X](size_tuple l) nth_cyclen subnK 1?ltnW//.
by rewrite  size_tuple modnn {2}/c (tnth_nth c).
Qed.

Lemma mono_cycle1P n (l: (necklace n.+1)): 
        reflect (cycle l 1 = l)  (l \in  (monocolor n.+1)).
Proof.
apply(iffP idP); first by apply:(mono_cycle 1).
by apply: cycle1_mono.
Qed.


(* Fermat Little Theorem *)
Variable p':nat.
Let p :=p'.+1.
Hypothesis p_prime: prime p.

Lemma aux3: forall (x y : 'I_p) (l: necklace p), (l \in (multicolor p)) -> cycle l x = cycle l y  -> x = y.
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
move:(cycle_back  l') =>/(cycle_gcd cycl'); rewrite size_tuple.
have ->:  gcdn d p = 1.
  rewrite gcdnC; apply/eqP; move:(prime_coprime d  p_prime); rewrite /coprime => ->.
  by rewrite gtnNdvd.
move/mono_cycle1P=> hl'.
have: cycle l' (p -x) = l.
  by rewrite /l' cycle_add subnK // 1?ltnW // -{2}[p](size_tuple l) cycle_back.
rewrite(mono_cycle _ hl')=> /val_inj ll'.
by move: ml; rewrite in_setC  -ll' hl'.
Qed.


Theorem cyclen_inj: forall (l: necklace p),  l \in (multicolor _)-> 
     injective (fun n : 'I_p => (cycle l n)).
Proof. by move=> l ml  x yl; apply: aux3. Qed.

Lemma cycle_multi n  (l: necklace p ):
         (l \in multicolor p)-> [tuple of (cycle l n)] \in multicolor p.
Proof.
rewrite inE =>lm; rewrite inE; move:lm; apply:contra.
move/mono_cycle1P; rewrite cycle_add -cycle_mod_length /=;
rewrite -(cycle_mod_length l n) size_tuple  => hcycle.
case:(boolP(l \in (multicolor _))); last by rewrite in_setC negbK.
have h1: ((1 + n) %% p'.+1) < p by rewrite ltn_pmod.
have h: (n %% p'.+1) < p by rewrite ltn_pmod. 
move/cyclen_inj =>/(_ (Ordinal h1) (Ordinal h) hcycle) /val_eqP /=.
by rewrite -{2}(add0n n) eqn_modDr !modn_small // prime_gt1.
Qed.

Lemma aux4: forall (l : necklace p) l' n, l \in (multicolor _) -> l' = (cycle  l n)->
    exists (k:'I_p) , l' = cycle l k.
Proof.
move=> l l' n ml.
rewrite -cycle_mod_length => ->.
by exists (Ordinal (ltn_pmod  n(prime_gt0 p_prime))); rewrite size_tuple.
Qed.


Theorem card_assoc: forall (l : necklace p), l \in (multicolor _) -> 
                     #|(associates  l)| = p.
Proof.
move=> l ml.
rewrite -[in RHS](card_ord p) /associates.
move:(@card_codom _  _(fun n:'I_p => [tuple of (cycle l n)]))=> <-; first last.
  by move => x y /eqP; rewrite -val_eqE => /eqP  H; rewrite (@cyclen_inj _ ml x y).
apply/eq_card=>x; rewrite !inE; apply/idP/idP.
  rewrite sim_sym; move/existsP=>[n /eqP  hn]; apply/codomP.
  exists (Ordinal (ltn_pmod n (prime_gt0 p_prime))).
  by apply/val_inj; rewrite hn /= -[p in _%%p](size_tuple l) cycle_mod_length.
move/codomP => [n hn]; rewrite sim_sym.
by apply/existsP; exists n; rewrite hn.
Qed.

Notation n := #|a|.

Theorem Fermat_little_th: p %| n^p - n.
Proof.
have ppos: p = p.-1.+1 by rewrite prednK // prime_gt0. 
rewrite ppos -(card_multi p.-1) -ppos.
pose part:=(@equivalence_partition (necklace p) (@similar p) (multicolor p)).
suff->: #|multicolor p|=#|part| * p by rewrite dvdn_mull.
have: partition part  (multicolor p).
  apply/equivalence_partitionP => x y z.
  by split; [rewrite sim_refl | apply:sim_sym_trans].
apply:card_uniform_partition =>x; rewrite /part/equivalence_partition.
case/imsetP=> l lm ->.
move:(card_assoc  lm); rewrite /associates =>  h.
rewrite -[in RHS]h; apply eq_card => y.
rewrite inE [in RHS]inE sim_sym; apply:andb_idl.
rewrite sim_sym; move/existsP=>[n /eqP ->].
by rewrite (cycle_multi  n lm).
Qed.

End TheNecklaceProof.

Section DirectNumberTheoreticProof.
Variable p':nat.
Let p :=p'.+1.
Hypothesis p_prime: prime p.


(* Theorem 7 *)
Theorem modnKr x y z :  ~~(p %| x) -> x * y = x * z %[mod p] -> y = z %[mod p].
Proof.
move=> px_coprime.
wlog lt_yz: y z / z <= y.
  move=> hwlog.
  case/orP: (leq_total z y); first by apply:hwlog.
  move=> hyz /eqP; rewrite eq_sym=>/eqP xyz_p.
  by apply:sym_eq; apply: hwlog.
move/eqP; rewrite eqn_mod_dvd; last by rewrite leq_mul2l lt_yz orbT.
rewrite -mulnBr Euclid_dvdM // (negbTE  px_coprime) /=.
by rewrite -eqn_mod_dvd=> // /eqP.
Qed.

(* residues *)

Definition residues := [set i:'I_p | i != ord0].
Definition residuen (i:nat) := (0 < i) && (i < p).


Definition row (a:'I_p) := 
    fun (x: 'I_p) =>(Ordinal (ltn_pmod (a * x) (prime_gt0 p_prime))).



Fact row_inj a:  (a != ord0) -> (injective (row a)).
Proof.
move => ha  [x hx] [y hy].
rewrite /row =>  /val_eqP /= /eqP hrow.
apply/val_eqP; rewrite /=  -(modn_small hx) -(modn_small hy).
apply/eqP; apply: (@modnKr a)=> //.
by rewrite gtnNdvd // lt0n.
Qed.

Theorem th8 a : a \in residues -> [set row a x | x in residues] = residues.
Proof.
rewrite inE =>ha.
apply/setP;apply/subset_cardP. 
  apply/eqP /imset_injP => y z hy hz; apply:(row_inj ha).
apply/subsetP => x.
move/imsetP=>[y]; rewrite !inE => hy hx; move:hy; apply/contraNN=>/eqP hx0.
move:hx; rewrite hx0.
have <-: row a ord0 = ord0.
  by  apply/val_eqP; rewrite /= muln0  mod0n.
by move/(row_inj ha) <-;apply/val_eqP; rewrite /row  muln0; apply/val_inj.
Qed.

Lemma pdivfact: forall n, n < p ->   ~~ (p %| n`!).
Proof.
elim=>[_| n Ihn hn]; first by rewrite fact0 Euclid_dvd1.
rewrite factS Euclid_dvdM // negb_or Ihn ?andbT ?gtnNdvd //.
by rewrite (ltn_trans (ltnSn n)).
Qed.

Theorem Fermat_little_th2:forall a, a \in residues -> a^(p-1) %% p = 1.
Proof.
move=> a ha.
rewrite -[in RHS](@modn_small 1 p) ?(prime_gt1) //.
apply:(@modnKr  p'`!); rewrite ?pdivfact //.
rewrite muln1.
have card_res: \prod_(i< p | i != ord0) i = p'`!.
  rewrite fact_prod.
  rewrite -(@big_mkord nat 1 muln p (fun i  => i!= 0) (fun x => x)).
  rewrite big_nat_cond (big_ltn_cond  (prime_gt0 p_prime)) /=.
  by apply:congr_big_nat=>// i //= /andP; case; rewrite lt0n => -> ->.
suff ->:  p'`! * a ^ (p - 1) = 
              \prod_(i in [set row a x | x in residues]) i %[mod p].
  rewrite -card_res.
  suff ->: \prod_(i < p | i != ord0) i =  \prod_(i in residues) i by rewrite th8.
  by apply/eq_bigl=> i; rewrite inE.
rewrite  big_imset /=; first last.
  by move=> x y hx hy; apply:row_inj; rewrite inE in ha.
have ->: \prod_(i in residues) ((a * i) %% p) = 
        \prod_(i < p | i != ord0)((a * i) %% p).
  by rewrite /residues; apply/eq_bigl =>i; rewrite inE.
have -> : \prod_(i < p| i!= ord0) ((a * i) %% p) 
       = (\prod_(i < p | i != ord0) (a * i))%[mod p].
  apply/eqP; elim/big_rec2: _ =>[//| i y1 y2 hi hy2].
  by rewrite modnMml -modnMm  (eqP hy2) modnMm.
rewrite big_split /= big_const /= iter_muln muln1.
by rewrite cardC1 card_ord subn1 mulnC card_res.
Qed.

End DirectNumberTheoreticProof.

From mathcomp Require Import all_fingroup.
From mathcomp Require Import algebra.zmodp.

Section ApplyingActions2Necklaces.

Notation cycle:= Top.cycle.
Variable a:finType.
Variable p': nat.
Let p := p'.+2.

Definition act  (l:(necklace  a p)) (k: 'Z_p) := [tuple of cycle l k].
Lemma actP : is_action [set: 'Z_p] act.
Proof.
apply: is_total_action. 
  by move=> l; apply/val_inj; rewrite /=  cycle_0.
rewrite /act /act_morph /=  => l k1 k2.
apply/val_inj; rewrite /=.
have {3}-> : (Zp_trunc p).+2 = (size l) by rewrite size_tuple.
by rewrite cycle_mod_length cycle_add addnC.
Qed.

Canonical cycle_action := Action actP.

Lemma th26: (afix cycle_action [set:'Z_p])= (monocolor a p).
Proof.
apply/setP=> l; apply/idP/idP; first last.
  move/mono_cycle; rewrite inE subTset=> lmono.
  apply/eqP /setP=> x; rewrite !inE /=.
  by apply/eqP/val_inj; rewrite /= lmono.
rewrite inE /=  subTset => /eqP /setP /= h.
apply/mono_cycle1P.
have h1: 1 < p by done.
move:(h (Ordinal h1)); rewrite !inE /=.
by move=> /eqP  /val_eqP /eqP.
Qed.

Fact th19 x: p =  #|orbit cycle_action [set:'Z_p]%G x| * #|('C_[set:'Z_p][x | cycle_action])%g|.
Proof.
rewrite card_orbit_in_stab; last by rewrite subEproper eqxx.
by rewrite cardsT card_ord.
Qed.

Fact aux19 x: (prime p) ->  (#|orbit cycle_action [set:'Z_p]%G x| == 1) || 
(#|orbit cycle_action [set:'Z_p]%G x| == p).
Proof.
move/primeP=>[]pgt1; apply; apply/dvdnP.
by exists #|('C_[set:'Z_p][x | cycle_action])%g|; rewrite [in LHS](th19 x) mulnC.
Qed.

(* Fermat's little theorem by Fixed Points *)
Notation n := #|a|.
Theorem Fermat_little_th3: prime p -> n^p = n %[mod p].
Proof.
move=>p_prime.
rewrite -card_necklace.
rewrite -(cardsC  (monocolor a p)) /=.
have hmulti:[acts [set: 'Z_p]%G, on (multicolor a p) | cycle_action].
  apply/subsetP=>x hx; rewrite !inE; apply/subsetP=> y hy.
  by rewrite inE cycle_multi.
move:(acts_sum_card_orbit hmulti).
move <-.
rewrite (eq_bigr (fun _ => p)).
  by rewrite big_const iter_addn_0 card_mono addnC mulnC modnMDl.
move=> i.
move/imsetP=>[] x; rewrite inE  => /negP hx ->.
by case/orP: (aux19 x p_prime)=> /eqP // /card_orbit1 /orbit1P; rewrite th26.
Qed.

End ApplyingActions2Necklaces.
