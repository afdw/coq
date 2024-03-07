Tracing
===

To enable tracing, set one or both of the following environment variables, and then run `coqtop` or `coqc` normally:
1. `TRACING_INTERACTIVE=1` to print information to the output;
2. `TRACING_FILE=trace.json` to append the results to the `trace.json` file. The extension of the file should be `.json`,
   unless `TRACING_COMPRESS=1` is set and `TRACING_SPLIT=1` is not, in which case it should be `.json.zst`.
   1. Use `TRACING_SPLIT=1` to split the output into files.
   2. Use `TRACING_COMPRESS=1` to compress the output files.

Use `TRACING_LOW_LEVEL=1` to generate low-level printing variants.
Use `TRACING_NO_EVENT=1` to suppress the generation of the detailed tactic trace.

The output has the following structure:
```ocaml
(** An absolute paths of a declaration (including module path), separated by periods. *)
type full_path = string

(** Globally named reference mentioned in a term or a tactic. *)
module Reference : sig
  type t =
    | Const of {path : full_path}
      (** A constant, which could be a theorem, like [ge] or [le_pred]. *)
    | Ind of {path : full_path}
      (** A (co)inductive type, like [nat]. *)
    | Construct of {ind_path : full_path; path : full_path}
      (** A (co)inductive constructor, like [S] (first field is the corresponding type). *)
end

(** Different variants of printing a term or a tactic. *)
module PrintingVariants : sig
  type t = {
    default : string; (** Default printing with all the notation and shortcuts. *)
    full_path : string; (** Same as above, but all the references are printed with full paths (including module paths). *)
    no_notations : string; (** Same as above, but without custom notations. *)
    low_level : string; (** With all the low-level contents possible. *)
    default_pretty : string; (** Same as [default], but with the formatting applied. *)
    references : Reference.t list; (** References mentioned. *)
  }
end

module Hyp : sig
  type kind =
    | Assumption
      (** [name : type_] *)
    | Definition of {value : PrintingVariants.t}
      (** [name : type_ := value] *)

  type t = {
    name : string;
    type_ : PrintingVariants.t;
    kind : kind;
  }
end

module Goal : sig
  type t = {
    hyps : Hyp.t list;
    concl : PrintingVariants.t;
  }
end

module Event : sig
  type tactic_kind =
    | Primitive of {s : string} (** For example, simple [refine] operations. *)
    | Builtin of {s : string} (** Tactic type which is present in the AST, or an argument type of a tactic. *)
    | Alias of {s : string} (** [Tactic Notation]. *)
    | ML of {s : string} (** Tactic implemented in OCaml. *)

  (** During the execution of a tactic, a trace is generated.
      It contains messages generated, well as information about
      how the tactic was executed in terms of more basic tactics.
      It can be visualized using the [Info n tac.] command,
      and here is layed out as a tree. *)
  type t =
    | Sequence of {elements : Event.t list}
      (** Events generated sequentially, one after another. *)
    | Dispatch of {
        goals_before : Goal.t list;
        branches : Event.t list;
      }
      (** Events generated in parallel in terms of goals,
          the list contains events about each of the current goals. *)
    | Tactic of {
        goals_before : Goal.t list;
        goals_after : Goal.t list;
        kind : tactic_kind;
        tactic : PrintingVariants.t;
        details : Event.t;
      }
      (** A tactic was generated, and is course of action is detailed in the event. *)
    | Message of {message : string}
      (** A message was produced. *)
end

(** A step in the proof. *)
module Step : sig
  type kind =
    | Tactic of {
        goal_selector : string; (** Goal selector, such as [all:]. *)
        tactic_raw : string; (** The tactic, almost exactly as it was written by the user. *)
        tactic : PrintingVariants.t; (** The tactic printed different ways. *)
        event : Event.t; (** How it executed. *)
      }
    | StartSubproof (** { *)
    | EndSubproof (** } *)
    | Bullet of {bullet : string} (** A proof bullet, for example "***". *)

  type t = {
    goals_before : Goal.t list; (** Goals before this step. *)
    goals_after : Goal.t list; (** Goals after this step. *)
    kind : kind;
  }
end

module Declaration : sig
  type outcome =
    | Admitted (** The theorem was admitted. *)
    | Proved (** The theorem was proved. *)
    | Exact (** An explicit proof term was given. *)
    | Abort (** The proof was aborted. *)
    | Fail (** Coq existed while proof was in progress. *)

  type kind =
    | Inductive
    | Constructor of {ind_path : full_path}
    | Assumption
    | Definition of {
        value : PrintingVariants.t;
        equations : PrintingVariants.t list;
      }
    | Interactive of {
        steps : Step.t list;
        outcome : outcome; (** What happened in the end. *)
      }

  type t = {
    path : full_path; (** Name of the declaration. *)
    type_ : PrintingVariants.t;
    kind : kind;
  }
end

module Trace : sig
  type t = {
    sub_filenames : string list; (** Paths to other trace files included in this one. *)
    declarations : Declaration.t list;
  }
end
```
