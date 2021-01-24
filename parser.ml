type expr =
  | Empty
  | Dot
  | Character of char
  | CharRange of char * char
  | Or of expr * expr
  | And of expr * expr
  | BoundedRange of expr * int * int
  | UnboundedRange of expr * int

(* Internal representation of the parser state. *)
type parser_state =
| State1 (* Base State. *)
| State2 (* Looking for quantifier. *)
| State3 (* Opened character class. *)
| State4 (* Looking for - in class. *)
| State5 (* Passed - in class, looking for final char in range. *)
(* More states to come: {} Quantifier definition. *)

let parse (inp:string) : expr =
  let rec _parse inp n state expr =
    let length = String.length inp in
    if n = length then expr
    else begin
      let current_char = inp.[n] in
      let transition = _parse inp (n + 1) in
      let remain = _parse inp n in
      match state with
      | State1 -> begin
        match current_char with
        | '.' -> transition State2 (And (Dot, expr))
        | '[' -> transition State3 (And (Empty, expr))
        | a -> transition State2 (And (Character a, expr))
      end
      | State2 -> begin
        match expr with
        | And (recent_expr, prev_expr) -> begin
          match current_char with
          | '?' -> transition State1 (And (BoundedRange (recent_expr, 0, 1), prev_expr))
          | '*' -> transition State1 (And (UnboundedRange (recent_expr, 0), prev_expr))
          | '+' -> transition State1 (And (UnboundedRange (recent_expr, 1), prev_expr))
          | _ -> remain State1 expr
        end
        | _ -> failwith "internal error in State 2"
      end
      | State3 -> begin
        match expr with
        | And (current_expr, prev_expr) -> begin
          match current_char with
          | '-' -> failwith "Parse failure: Unexpected `-'."
          | ']' -> transition State1 expr
          | a -> transition State4 (And (Or (Character a, current_expr), prev_expr))
        end
        | _ -> failwith "internal error in State 3"
      end
      | State4 -> begin
        match expr with
        | And (Or (recent_expr1, recent_expr2), prev_expr) -> begin
          match current_char with
          | '-' -> transition State5 expr
          | _ -> remain State3 expr
        end
        | _ -> failwith "internal error in State 4"
      end
      | State5 -> begin
        match expr with 
        | And (Or (Character c, recent_expr2), prev_expr) -> begin
          transition State3 (And (Or (CharRange (c, current_char), recent_expr2), prev_expr))
        end
        | _ -> failwith "internal error in State 5"
      end
    end in
  _parse inp 0 State1 Empty
