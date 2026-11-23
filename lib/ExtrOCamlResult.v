From Corelib Require Import Extraction.
From Result Require Export Result.

(** Extract [result] to OCaml's standard result type.
    Qualified names avoid clashes with an extracted module named [Result]. *)
Extraction Language OCaml.
Extract Inductive result => "Stdlib.result" [ "Stdlib.Ok" "Stdlib.Error" ].
