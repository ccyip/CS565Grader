
(** Exercise: 2 points (plus_assoc) *)
Theorem plus_assoc : forall m n p : nat,
    m + (n + p) = (m + n) + p.
Proof.
  intros.
Admitted.

Module Number.
  (** Exercise: 1 point (plus_n_O) *)
  Theorem plus_n_O : forall n : nat,
      n = n + 0.
  Proof.
    induction n; auto.
  Qed.

End Number.

(** Bonus question: 5 bonus points (plus_very_hard) *)
Theorem plus_very_hard : 0 + 0 = 0.
Proof.
  reflexivity.
Qed.

(** We define a double function. *)
Fixpoint double (n:nat) :=
  match n with
  | O => O
  | S n' => S (S (double n'))
  end.

(** Also the even predicate. *)
Inductive ev : nat -> Prop :=
| ev_0 : ev 0
| ev_SS : forall n : nat, ev n -> ev (S (S n)).

(** Exercise: 3 points (plus_plus_double) *)
Theorem plus_plus_double : forall n : nat,
    n + n = double n.
Proof.
  induction n; simpl; auto.
  rewrite <- IHn.
Admitted.

(** Exercise: 3 points (double_ev) *)
Theorem double_ev : forall n : nat,
    ev (double n).
Proof.
  induction n.
  constructor.
  simpl. constructor. auto.
Qed.

(** Exercise: 1 points (test_plus) *)
Example test_plus1 : plus 1 1 = 2.
Proof.
  reflexivity. Qed.

Example test_plus2 : plus 1 2 = 3.
Proof.
  reflexivity. Qed.

Example test_plus3 : plus 1 3 = 4.
Proof.
  reflexivity. Qed.


(** Exercise: 1 points (test_double) *)
Example test_double1 : double 1 = 2.
Proof.
  reflexivity. Qed.

Example test_double2 : double 2 = 4.
Proof.
  reflexivity. Qed.

Example test_double3 : double 3 = 6.
Proof.
  reflexivity. Qed.

(* Exercise: 2 point (leb) *)
Fixpoint leb (n m : nat) : bool :=
  match n with
  | 0 => true
  | S n' => match m with
            | 0 => true
            | S m' => leb n' m'
            end
  end.

(* Exercise: 2 point (leb_refl) *)
Theorem leb_refl : forall n, leb n n = true.
Proof.
  induction n; eauto.
Qed.

(* Exercise: 3 point (leb_succ) *)
(* State and prove that [n] is smaller than or equal to [S n]. *)
Theorem leb_succ :
  forall n, leb (S n) n = true.
Proof.
  induction n; eauto.
Qed.

(* Exercise: 3 point (leb_double) *)
(* State and prove that [n] is smaller than or equal to [double n]. *)
Theorem leb_double :
  forall n, leb n (double n) = true.
Proof.
  intros n. generalize (double n).
  induction n.
  destruct n; reflexivity.
  destruct n0; eauto.
Qed.

(* Exercise: 10 point (plus_1) *)
Theorem plus_1 : forall n, plus n 1 = S n.
Proof.
  induction n; simpl. reflexivity.
  simpl.
Admitted.
