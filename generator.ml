open Ast

let rec generate_expr = function
  | Empty ->
      generate_empty ()
  | Dot ->
      generate_dot ()
  | Character a ->
      generate_char a
  | Or lst ->
      generate_or lst
  | And lst ->
      generate_and lst
  | Multiple (expr, quant) ->
      generate_multiple expr quant
  | BoundedRange (expr, start_quant, end_quant) ->
      generate_bounded_range expr start_quant end_quant
  | UnboundedRange (expr, start_quant) ->
      generate_unbounded_range expr start_quant

and generate_empty () = ""

and generate_dot () =
  let generated_char = Random.int 95 + 32 |> char_of_int in
  generate_expr (Character generated_char)

and generate_char = String.make 1

and generate_or lst =
  let ind = lst |> List.length |> Random.int in
  ind |> List.nth lst |> generate_expr

and generate_and lst =
  (* It is completely possible to use List.fold_left with the ^ operator
   * instead, but String.concat is much faster than the ^ operator. *)
  lst |> List.rev_map generate_expr |> String.concat ""

and generate_multiple expr quant =
  let rec _gen n acc expr =
    if n = 0 then acc else _gen (n - 1) (generate_expr expr :: acc) expr
  in
  expr |> _gen quant [] |> String.concat ""

and generate_bounded_range expr start_quant end_quant =
  let _end_quant =
    if start_quant = 0 then end_quant + 1 else end_quant - start_quant
  in
  let quant = start_quant + Random.int _end_quant in
  generate_expr (Multiple (expr, quant))

and generate_unbounded_range expr start_quant =
  (* For practical reasons, I'm limiting unbounded ranges to 255 characters. *)
  generate_expr (BoundedRange (expr, start_quant, 255))

let generate = generate_expr
