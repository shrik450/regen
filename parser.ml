type expr =
  | Empty
  | Dot
  | Character of char
  (* The usage of expr lists here isn't strictly necessary, and they can be 
   * replaces with expr * expr instead. However, using expr lists here makes 
   * generation convenient later on by letting us use List module methods such
   * as List.length, List.nth and List.rev_map. *)
  | Or of expr list
  | And of expr list
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

let unexpected_error =
  Printf.sprintf "Parse failure: Unexpected `%c' at col %d"

let out_of_range_error =
  Printf.sprintf "Parse failure: Characters out of range at col %d."

let rec _parse inp n state expr =
  let len = String.length inp in
  if len = n then expr
  else begin
    match state with
    | State1 -> _parse_state_1 inp n expr
    | State2 -> _parse_state_2 inp n expr
    | State3 -> _parse_state_3 inp n expr
    | State4 -> _parse_state_4 inp n expr
    | State5 -> _parse_state_5 inp n expr
  end

and _parse_state_1 inp n = function
  | And lst -> begin
    let current_char = inp.[n] in
    match current_char with
    | '.' -> _parse inp (n + 1) State2 (And (Dot::lst))
    | '[' -> _parse inp (n + 1) State3 (And ((Or [])::lst))
    | a   -> _parse inp (n + 1) State2 (And ((Character a)::lst))
  end
  | _ -> failwith "internal failure in state 1."

and _parse_state_2 inp n expr =
  match expr with
  | And (h::t) -> begin
    let current_char = inp.[n] in
    match current_char with
    | '?' -> begin
      let new_h = BoundedRange (h, 0, 1) in
      _parse inp (n + 1) State1 (And (new_h::t))
    end
    | '*' -> begin
      let new_h = UnboundedRange (h, 0) in
      _parse inp (n + 1) State1 (And (new_h::t))
    end
    | '+' -> begin
      let new_h = UnboundedRange (h, 1) in
      _parse inp (n + 1) State1 (And (new_h::t))
    end
    | _   -> _parse inp n State1 expr
  end
  | _ -> failwith "internal failure in state 2."

and _parse_state_3 inp n expr =
  match expr with
  | And (h::t) -> begin
    let current_char = inp.[n] in
    match current_char with
    (* Regex engines aren't consistent across how a leading - is treated. 
     * In some implementations it is assumed to be a literal, while in others
     * it creates a range that doesn't match anything. To make our handling of
     * this simple, we'll simply raise an error. *)
    | '-' -> failwith @@ unexpected_error current_char (n + 1)
    | ']' -> _parse inp (n + 1) State1 expr
    | a   -> begin
      match h with
      | Or lst -> begin
        let new_h = Or ((Character a)::lst) in
        _parse inp (n + 1) State4 (And (new_h::t))
      end
      | _ -> failwith "internal failure in state 3: no OR list."
    end
  end
  | _ -> failwith "internal failure in state 3: no AND list."

and _parse_state_4 inp n expr =
  let current_char = inp.[n] in
  match current_char with
  | '-' -> _parse inp (n + 1) State5 expr
  | _   -> _parse inp n State3 expr

and _parse_state_5 inp n expr =
  match expr with
  | And (((Or ((Character c)::tl1)))::tl2) -> begin
    let current_char = inp.[n] in
    let init = int_of_char c in
    let diff =  (int_of_char current_char) - init in
    if diff <= 0
    then failwith @@ out_of_range_error (n + 1)
    else begin
      
      let rec add_chars init n lst = 
        (* Don't forget to keep the current character! *)
        if n = -1 then lst
        else add_chars init (n - 1) @@ (Character (char_of_int (init + n)))::lst in

      let new_h = Or (add_chars init diff tl1) in
        _parse inp (n + 1) State3 (And (new_h::tl2)) 
    end
  end
  | _ -> failwith "internal error in state 5."

let parse (inp:string) : expr =
  _parse inp 0 State1 (And [])
