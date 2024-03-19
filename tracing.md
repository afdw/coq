Tracing
===

To enable tracing, set one or both of the following environment variables, and then run `coqtop` or `coqc` normally:
1. `TRACING_INTERACTIVE=1` to print information to the output;
2. `TRACING_FILE=trace.json` to append the results to the `trace.json` file. The extension of the file should be `.json`,
   unless `TRACING_COMPRESS=1` is set and `TRACING_SPLIT=1` is not, in which case it should be `.json.zst`.
   1. Use `TRACING_SPLIT=1` to split the output into files.
   2. Use `TRACING_COMPRESS=1` to compress the output files.

The output has the following structure:
```ocaml
(** An absolute paths of a declaration (including module path), separated by periods. *)
type full_path = string

(** Globally named feature mentioned in a term or a tactic. *)
module Feature : sig
  type t =
    | FeatureConstRef of {path: full_path}
      (** A constant, which could be a theorem, like [ge] or [le_pred]. *)
    | FeatureIndRef of {path: full_path}
      (** A (co)inductive type, like [nat]. *)
    | FeatureConstructRef of {ind_path: full_path; path: full_path}
      (** A (co)inductive constructor, like [S] (first field is the corresponding type). *)
end

(** Different variants of printing a term or a tactic. *)
module PrintingVariants : sig
  type t = {
    default : string; (** Default printing with all the notation and shortcuts. *)
    full_path : string; (** Same as above, but all the references are printed with full paths (including module paths). *)
    no_notations : string; (** Same as above, but without custom notations. *)
    low_level : string; (** With all the low-level contents possible. *)
    features : Feature.t list; (** Features mentioned. *)
  }
end

module Goal : sig
  type hyp =
    | Assum of {name : string; type_ : PrintingVariants.t}
      (** [name : type_] *)
    | Def of {name : string; type_ : PrintingVariants.t; value : PrintingVariants.t}
      (** [name : type_ := value] *)

  type t = {
    hyps : hyp list;
    concl : PrintingVariants.t;
  }
end

module Info : sig
  (** During the execution of a tactic, a trace is generated.
      It contains messages generated, well as information about
      how the tactic was executed in terms of more basic tactics.
      It can be visualized using the [Info n tac.] command,
      and here is layed out as a tree. *)
  type event =
    | EventSeq of event list
      (** Events generated sequentially, one after another. *)
    | EventDispatch of event list
      (** Events generated in parallel in terms of goals,
          the list contains events about each of the current goals. *)
    | EventTactic of Goal list * PrintingVariants.t * event
      (** A tactic was generated, and is course of action is detailed in the event. *)
    | EventMessage of string
      (** A message was produced. *)
end

(** A proof bullet, for example, [Star 3] is "***". *)
module Proof_bullet : sig
  type t =
    | Dash of int
    | Star of int
    | Plus of int
end

(** A step in the proof. *)
module Step : sig
  type kind =
    | Tactic of {
        raw : string; (** The tactic, almost exactly as it was written by the user. *)
        tactic : PrintingVariants.t; (** The tactic printed different ways. *)
        event : Info.event; (** How it executed. *)
      }
    | StartSubproof (** { *)
    | EndSubproof (** } *)
    | Bullet of Proof_bullet.t

  type t = {
    goals_before : Goal.t list; (** Goals before this step. *)
    kind: kind;
  }
end

module Theorem : sig
  type outcome =
    | Admitted (** The theorem was admitted. *)
    | Proved (** The theorem was proved. *)
    | Exact (** An explicit proof term was given. *)

  type t = {
    path : full_path;
      (** Name of the theorem. *)
    steps : Step.t list;
      (** The statement is the [goals_before] of the first element of this list. *)
    outcome : outcome;
      (** What happened in the end. *)
  }
end

module Trace : sig
  type t = {
    theorems : Theorem.t list;
  }
end
```
