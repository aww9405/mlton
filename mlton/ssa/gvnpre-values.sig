(*
* VALUES
*)

signature VALUE = sig
    datatype t = T of int
    val new: unit -> t
    val equals: t * t -> bool
end

(*
* VALUE TABLES
* Keeps track of the equivalence of expressions
* by mapping them to tokens called "values";
* two expressions are equivalent if they map to the same token.
*)

signature VALUE_TABLE = sig
    type t
    val new: unit -> t
    val add: t * Expr.t * Value.t -> Value.t
    val lookup: t * Expr.t -> Value.t option
    val lookup_or_add: t * Expr.t -> Value.t option
end

(*
* VALUE REPRESENTATIVE SETS
* A set of expressions indexed by value.
* An expression cannot be inserted if its corresponding value
*   is already represented in the set by another expression.
*   However, the expression can replace the other expression.
*)

signature VALUE_SET = sig
    type t
    val new: ValueTable.t -> t
    val get: t * Value.t -> Expr.t option
    val val_insert: t * Expr.t -> Value.t option
    val val_replace: t * Expr.t -> Value.t option
end

