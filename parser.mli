type expr =
    Empty
  | Dot
  | Character of char
  | CharRange of char * char
  | Or of expr * expr
  | And of expr * expr
  | BoundedRange of expr * int * int
  | UnboundedRange of expr * int

(* [parse regexp] is the compiled form of [regexp]. *)
val parse : string -> expr
