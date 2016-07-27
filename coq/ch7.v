From mathcomp Require Import all_ssreflect perm fingroup.

Inductive windrose : predArgType := N | S | E | W.

Definition w2o w : 'I_4 :=
  match w with
  | N => inord 0
  | S => inord 1
  | E => inord 2
  | W => inord 3
  end.

Definition o2w (o : 'I_4) :=
  match val o with
  | 0 => N
  | 1 => S
  | 2 => E
  | 3 => W
  | _ => N
  end.

Lemma pcan_wo4 : cancel w2o o2w.
Proof. by case; rewrite /o2w /= inordK. Qed.

Definition windrose_eqMixin := CanEqMixin pcan_wo4.
Canonical windrose_eqType := EqType windrose windrose_eqMixin.
Definition windrose_choiceMixin := CanChoiceMixin pcan_wo4.
Canonical windrose_choiceType := ChoiceType windrose windrose_choiceMixin.
Definition windrose_countMixin := CanCountMixin pcan_wo4.
Canonical windrose_countType := CountType windrose windrose_countMixin.
Definition windrose_finMixin := CanFinMixin pcan_wo4.
Canonical windrose_finType := FinType windrose windrose_finMixin.

Check [finType of windrose].

Lemma ord4_is_w : cancel o2w w2o.
Proof.
move=> x; apply: val_inj; case: x.
by do 5! [ case=> [?|//]; first by rewrite /= inordK ].
Qed.

Lemma test : (N != S) && (N \in windrose) && (#| windrose | == 4).
Proof.
case: eqP => //= _; rewrite -[4]card_ord.
rewrite -(card_image (can_inj pcan_wo4)).
apply/eqP; apply: eq_card=> o; rewrite inE.
by apply/imageP; exists (o2w o) => //=; rewrite ord4_is_w.
Qed.



