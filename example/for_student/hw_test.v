Require Export String.
Open Scope string.

Ltac start m := idtac "----" m.
Ltac nl := idtac "".
Ltac exc x := idtac "Exercise:" x.
Ltac itm x := idtac "Sub-exercise:" x.
Ltac pt x := idtac "Possible points:" x.
Ltac ass := idtac "Assumptions:".

Parameter MISSING: Type.

Ltac check_type A B :=
    match type of A with
    | context[MISSING] => gfail 1 "Type missing for" A
    | ?T => unify T B
    end.

Require hw.

(* See the test file in the "for_grader" directory for command usages. *)

Goal True.
  start "hw".
  nl.

  exc "plus_assoc".
  pt 2.
  check_type @hw.plus_assoc (forall m n p : nat, m + (n + p) = m + n + p).
  ass.
  Print Assumptions hw.plus_assoc.
  nl.

  exc "Number.plus_n_O".
  pt 1.
  check_type @hw.Number.plus_n_O (forall n : nat, n = n + 0).
  ass.
  Print Assumptions hw.Number.plus_n_O.
  nl.

  exc "plus_very_hard".
  pt 5.
  check_type @hw.plus_very_hard (0 + 0 = 0).
  ass.
  Print Assumptions hw.plus_very_hard.
  nl.

  exc "plus_plus_double".
  pt 3.
  check_type @hw.plus_plus_double (forall n : nat, n + n = hw.double n).
  ass.
  Print Assumptions hw.plus_plus_double.
  nl.

  exc "double_ev".
  pt 3.
  check_type @hw.double_ev (forall n : nat, hw.ev (hw.double n)).
  ass.
  Print Assumptions hw.double_ev.
  nl.

  exc "#test_plus".
  pt 1.
  nl.

  itm "test_plus1".
  check_type @hw.test_plus1 (1 + 1 = 2).
  ass.
  Print Assumptions hw.test_plus1.
  nl.

  itm "test_plus2".
  check_type @hw.test_plus2 (1 + 2 = 3).
  ass.
  Print Assumptions hw.test_plus2.
  nl.

  itm "test_plus3".
  check_type @hw.test_plus3 (1 + 3 = 4).
  ass.
  Print Assumptions hw.test_plus3.
  nl.

  exc "#test_double".
  pt 1.
  check_type @hw.test_double1 (hw.double 1 = 2).
  check_type @hw.test_double2 (hw.double 2 = 4).
  check_type @hw.test_double3 (hw.double 3 = 6).
  ass.
  Print Assumptions hw.test_double1.
  Print Assumptions hw.test_double2.
  Print Assumptions hw.test_double3.
  nl.

  exc "leb".
  pt 2.
  check_type @hw.leb (nat -> nat -> bool).
  nl.

  exc "leb_refl".
  pt 2.
  ass.
  Print Assumptions hw.leb_refl.
  nl.

  exc "leb_succ".
  pt 3.
  Check hw.leb_succ.
  ass.
  Print Assumptions hw.leb_succ.
  nl.

  exc "leb_double".
  pt 3.
  Check hw.leb_double.
  ass.
  Print Assumptions hw.leb_double.
  nl.

  exc "plus_1".
  pt 10.
  ass.
  Print Assumptions hw.plus_1.
  nl.

Abort.
