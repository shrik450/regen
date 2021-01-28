open Parser

let rec generate_expr expr =
  let func =
    match expr with
    | Empty -> generate_empty
    | Dot -> generate_dot
    | Character _ -> generate_char
    | Or _ -> generate_or
    | And _ -> generate_and
    | Multiple (_, _) -> generate_multiple
    | BoundedRange (_, _, _) -> generate_bounded_range
    | UnboundedRange (_, _) -> generate_unbounded_range
  in
  func expr

and generate_empty Empty = ""

and generate_dot Dot =
  let randint = Random.int 95 in
  let generated_char = char_of_int (randint + 32) in
  String.make 1 generated_char

and generate_char (Character a) = String.make 1 a

and generate_or (Or lst) =
  lst |> List.length |> Random.int |> List.nth lst |> generate_expr

and generate_and (And lst) =
  (* It is completely possible to use List.fold_left with the ^ operator 
   * instead, but String.concat is much faster than the ^ operator. *)
  lst |> List.rev_map generate_expr |> String.concat ""

and generate_multiple (Multiple (expr, quant)) =
  let rec _gen n acc expr =
    if n = 0 then acc else _gen (n - 1) (generate_expr expr :: acc) expr
  in
  expr |> _gen quant [] |> String.concat ""

and generate_bounded_range (BoundedRange (expr, start_quant, end_quant)) =
  let quant = start_quant + Random.int (end_quant + 1) in
  generate_expr (Multiple (expr, quant))

and generate_unbounded_range (UnboundedRange (expr, start_quant)) =
  (* For practical reasons, I'm limiting unbounded ranges to 255 characters. *)
  generate_expr (BoundedRange (expr, start_quant, 255))


let generate str =
  str |> Parser.parse |> generate_expr
