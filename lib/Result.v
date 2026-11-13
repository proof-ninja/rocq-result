Inductive result (A E: Set) :=
| Ok (x : A)
| Error (error : E).

Arguments Ok [A E].
Arguments Error [A E].


(** Return the successful value, or [default] if the result is an error. *)
Definition value {A E : Set} (r : result A E) (default : A) : A :=
  match r with
  | Ok x => x
  | Error _ => default
  end.

(** Apply [f] to a successful value, or propagate the error. *)
Definition bind {A B E : Set}
    (r : result A E) (f : A -> result B E) : result B E :=
  match r with
  | Ok x => f x
  | Error e => Error e
  end.

(** Flatten a nested result, preserving errors from either layer. *)
Definition join {A E : Set} (r : result (result A E) E) : result A E :=
  match r with
  | Ok inner => inner
  | Error e => Error e
  end.

(** Transform a successful value with [f], preserving errors. *)
Definition map {A B E : Set}
    (f : A -> B) (r : result A E) : result B E :=
  match r with
  | Ok x => Ok (f x)
  | Error e => Error e
  end.

(** Pair successful values, returning the left error if both results fail. *)
Definition product {A B E : Set}
    (r1 : result A E) (r2 : result B E) : result (A * B) E :=
  match r1 with
  | Ok x =>
      match r2 with
      | Ok y => Ok (x, y)
      | Error e => Error e
      end
  | Error e => Error e
  end.

(** Transform an error with [f], preserving successful values. *)
Definition map_error {A E F : Set}
    (f : E -> F) (r : result A E) : result A F :=
  match r with
  | Ok x => Ok x
  | Error e => Error (f e)
  end.

(** Handle a result with the corresponding success or error function. *)
Definition fold {A E C : Set}
    (ok : A -> C) (error : E -> C) (r : result A E) : C :=
  match r with
  | Ok x => ok x
  | Error e => error e
  end.

(** Apply [f] to a successful value, returning [tt] on error. *)
Definition iter {A E : Set}
    (f : A -> unit) (r : result A E) : unit :=
  match r with
  | Ok x => f x
  | Error _ => tt
  end.

(** Apply [f] to an error, returning [tt] on success. *)
Definition iter_error {A E : Set}
    (f : E -> unit) (r : result A E) : unit :=
  match r with
  | Ok _ => tt
  | Error e => f e
  end.

(** Test whether the result is successful. *)
Definition is_ok {A E : Set} (r : result A E) : bool :=
  match r with
  | Ok _ => true
  | Error _ => false
  end.

(** Test whether the result is an error. *)
Definition is_error {A E : Set} (r : result A E) : bool :=
  match r with
  | Ok _ => false
  | Error _ => true
  end.

(** Convert a successful value to [Some], or an error to [None]. *)
Definition to_option {A E : Set} (r : result A E) : option A :=
  match r with
  | Ok x => Some x
  | Error _ => None
  end.

(** Convert a successful value to a singleton list, or an error to an empty list. *)
Definition to_list {A E : Set} (r : result A E) : list A :=
  match r with
  | Ok x => cons x nil
  | Error _ => nil
  end.

(** Compare results using the supplied boolean comparisons.
    Agreement with propositional equality requires correctness of both comparisons. *)
Definition eqb {A E : Set}
    (ok : A -> A -> bool) (error : E -> E -> bool)
    (r1 r2 : result A E) : bool :=
  match r1, r2 with
  | Ok x, Ok y => ok x y
  | Error e, Error f => error e f
  | _, _ => false
  end.
