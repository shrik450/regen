type expr =
  | Empty
  | Dot
  | Character of char
  | Or of expr list
  | And of expr list
  | BoundedRange of expr * int * int
  | UnboundedRange of expr * int

(* [parse regexp] is the compiled form of [regexp]. *)
val parse : string -> expr
