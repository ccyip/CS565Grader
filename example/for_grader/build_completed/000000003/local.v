Require Export String.
Open Scope string.

(** Fill in comments (string) here if we have any for the student. It will
appear in feedback file, e.g., *)
(**
[[
Definition Comment := "Your homework does not compile!".
]] *)
Definition Comment := tt.

(** Apply penalty in percentage. It is usually used when the homework does not
compile. The value should be a string, with a number from 0 to 100, e.g., *)
(**
[[
Definition Penalty := "10".
]]
This applies 10% penalty.
*)
Definition Penalty := tt.

(** We list all the manual grade items here. *)
(** The definition could be in one of the following forms:

1. [tt]: this exercise is not graded yet. We do not need to grade every one
manually because (hopefully) most of them can be graded automatically.

2. of type [bool * string]: a boolean grade and a comment. They get full mark if
[true], or zero if [false]. e.g., [(false, "completely wrong")], [(true, "you
did well!")]. You can leave the comment string empty of course. I usually leave
useful comments to point out their mistakes.

3. of type [nat * string]: a numeric grade and a comment. e.g., [(1, "the first
case is correct but the rest is wrong")], [(2, "so close! the last case is
wrong")].

*)
Definition mg_leb := (1, "You should return false when [m = 0]").
Definition mg_leb_refl := (1, "Your definition of leb is wrong, making this proof meaningless").
Definition mg_leb_succ := (false, "The statement is wrong").
Definition mg_leb_double_statement := tt.
Definition mg_leb_double := (false, "Your definition of leb is wrong, making this proof meaningless").
Definition mg_plus_1 := (5, "No comment").

(** DO NOT remove these placeholders, though they are not used in most cases. In
my experience, sometimes we may think an exercise does not need manual grading.
But then for whatever reason, we need to turn it into a manual grade item. For
example, the students might complain and we need to give them partial credits
now. However, because [local.v] will not be overwritten (you don't want to lose
your manual graded scores) when we update the test script and re-prepare the
build, we can not simply add another manual grade item to [local.v]. In that
case, we can use one of these as a stand-in. *)
Definition mg_exc1 := tt.
Definition mg_exc2 := tt.
Definition mg_exc3 := tt.
Definition mg_exc4 := tt.
Definition mg_exc5 := tt.
