type expr =
  | Empty
  | Dot
  | Character of char
  | Or of expr list
  | And of expr list
  | Multiple of expr * int
  | BoundedRange of expr * int * int
  | UnboundedRange of expr * int
