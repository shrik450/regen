type expr =
  | Empty
  | Dot
  | Character of char
  (* The usage of expr lists here isn't strictly necessary, and they can be 
   * replaced with expr * expr instead. However, using expr lists here makes 
   * generation convenient later on by letting us use List module methods such
   * as List.length, List.nth and List.rev_map. *)
  | Or of expr list
  (* Also, Or lists should more appropriately be sets of expressions. Consider
   * The class [[ca-z]]. In this case, the expression [Character c] would 
   * appear twice in the output AST, but it shouldn't be any more likely than 
   * the other characters in some viewpoints. However, I think using a list is 
   * fine, since if a user goes out of their way to specify characters multiple
   * times in a class, I think they should get a higher chance of getting it. *)
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
  if len = n then begin
    match state with
    (* While State2 is expecting a quantifier, a lack of a quantifier never
     * hurt anyone. *)
    | State1 | State2 -> expr
    | State3 | State4 | State5 -> failwith "Unexpected EOF, expecting `]'."
  end
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
    | '*' | '+' | '?' -> failwith @@ unexpected_error current_char (n + 1)
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
    match current_char with
    (* As was the case with a leading -, a trailing - also seems to be a bit
     * weird. Again, we'll just raise an error instead. *)
    | ']' -> failwith @@ unexpected_error ']' (n + 1)
    | a   -> begin
      let init = int_of_char c in
      let diff =  (int_of_char a) - init in
      if diff <= 0
      then failwith @@ out_of_range_error (n + 1)
      else begin
        
        let rec add_chars init n lst = 
          (* Don't forget to keep the current character! *)
          if n = -1 then lst
          else add_chars init (n - 1) @@ (Character (char_of_int (init + n)))::lst in

        (* Note that [add_chars] adds characters in "forward" order - i.e., a-c is
        * added as [Character a; Character b; Character c]. This is important 
        * because in all other cases, we add them in reverse order as we 
        * progress through the input string. How the expressions are ordereed
        * matters in an And list, since we need to output a string that matches 
        * the ordering of the input regex. However, in this case it doesn't make
        * a difference, since the output is an Or expression. *)
        let new_h = Or (add_chars init diff tl1) in
          _parse inp (n + 1) State3 (And (new_h::tl2)) 
      end
    end
  end
  | _ -> failwith "internal error in state 5."

let parse (inp:string) : expr =
  _parse inp 0 State1 (And [])
