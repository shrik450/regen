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
  | Multiple of expr * int
  | BoundedRange of expr * int * int
  | UnboundedRange of expr * int

(* Internal representation of the parser state. *)
type parser_state =
  (* Base State. *)
  | State1
  (* Looking for quantifier. *)
  | State2
  (* Opened character class. *)
  | State3
  (* Looking for - in class. *)
  | State4
  (* Passed - in class, looking for final char in range. *)
  | State5
  (* Passed { as a quantifier def start. *)
  | State6
  (* Passed , in a quantifier def. *)
  | State7
  (* Passed | in State 1. *)
  | State8

let unexpected_error = Printf.sprintf "Parse failure: Unexpected `%c' at col %d"

let out_of_range_error =
  Printf.sprintf "Parse failure: Invalid range at col %d."

let numlen n = n |> string_of_int |> String.length

let rec _parse inp n state expr =
  let len = String.length inp in
  if len = n then
    match state with
    (* While State2 is expecting a quantifier, a lack of a quantifier never
     * hurt anyone. *)
    | State1 | State2 ->
        expr
    | State3 | State4 | State5 ->
        failwith "Unexpected EOF, expecting `]'."
    (* TODO: Implement conversion to literal if this happens. *)
    | State6 | State7 ->
        failwith "Unexpected EOF, expecting `}'."
    (* TODO: Implement conversion to literal in this case. *)
    | State8 ->
        failwith "Unexpected EOF, expecting any character."
  else
    let func =
      match state with
      | State1 ->
          _parse_state_1
      | State2 ->
          _parse_state_2
      | State3 ->
          _parse_state_3
      | State4 ->
          _parse_state_4
      | State5 ->
          _parse_state_5
      | State6 ->
          _parse_state_6
      | State7 ->
          _parse_state_7
    in
    func inp n expr

and _parse_state_1 inp n = function
  | And lst -> (
      let current_char = inp.[n] in
      match current_char with
      | '*' | '+' | '?' ->
          failwith @@ unexpected_error current_char (n + 1)
      | '.' ->
          _parse inp (n + 1) State2 (And (Dot :: lst))
      | '[' ->
          _parse inp (n + 1) State3 (And (Or [] :: lst))
      | a ->
          _parse inp (n + 1) State2 (And (Character a :: lst))
    )
  | _ ->
      failwith "internal failure in state 1."

and _parse_state_2 inp n expr =
  match expr with
  | And (h :: t) -> (
      let current_char = inp.[n] in
      match current_char with
      | '?' ->
          let new_h = BoundedRange (h, 0, 1) in
          _parse inp (n + 1) State1 (And (new_h :: t))
      | '*' ->
          let new_h = UnboundedRange (h, 0) in
          _parse inp (n + 1) State1 (And (new_h :: t))
      | '+' ->
          let new_h = UnboundedRange (h, 1) in
          _parse inp (n + 1) State1 (And (new_h :: t))
      | '{' ->
          _parse inp (n + 1) State6 expr
      | _ ->
          _parse inp n State1 expr
    )
  | _ ->
      failwith "internal failure in state 2."

and _parse_state_3 inp n expr =
  match expr with
  | And (h :: t) -> (
      let current_char = inp.[n] in
      match current_char with
      | ']' ->
          _parse inp (n + 1) State2 expr
      (* A leading - is also treated as a literal. *)
      | a -> (
        match h with
        | Or lst ->
            let new_h = Or (Character a :: lst) in
            _parse inp (n + 1) State4 (And (new_h :: t))
        | _ ->
            failwith "internal failure in state 3: no OR list."
      )
    )
  | _ ->
      failwith "internal failure in state 3: no AND list."

and _parse_state_4 inp n expr =
  let current_char = inp.[n] in
  match current_char with
  | '-' ->
      _parse inp (n + 1) State5 expr
  | _ ->
      _parse inp n State3 expr

and _parse_state_5 inp n expr =
  match expr with
  | And (Or (Character c :: tl1) :: tl2) -> (
      let current_char = inp.[n] in
      match current_char with
      (* As was the case with a leading -, a trailing - also seems to be a bit
       * weird. Again, we'll just raise an error instead. *)
      | ']' ->
          failwith @@ unexpected_error ']' (n + 1)
      | a ->
          let init = int_of_char c in
          let diff = int_of_char a - init in
          if diff <= 0 then failwith @@ out_of_range_error (n + 1)
          else
            let rec add_chars init n lst =
              (* Don't forget to keep the current character! *)
              if n = -1 then lst
              else
                add_chars init (n - 1)
                @@ (Character (char_of_int (init + n)) :: lst)
            in
            (* Note that [add_chars] adds characters in "forward" order - i.e., a-c is
             * added as [Character a; Character b; Character c]. This is important 
             * because in all other cases, we add them in reverse order as we 
             * progress through the input string. How the expressions are ordereed
             * matters in an And list, since we need to output a string that matches 
             * the ordering of the input regex. However, in this case it doesn't make
             * a difference, since the output is an Or expression. *)
            let new_h = Or (add_chars init diff tl1) in
            _parse inp (n + 1) State3 (And (new_h :: tl2))
    )
  | _ ->
      failwith "internal error in state 5."

and _parse_state_6 inp n expr =
  match expr with
  | And (hd :: tl) -> (
      let current_char = inp.[n] in
      match current_char with
      | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0' ->
          let quant = int_of_char current_char - 48 (* 48 is ascii for 0. *) in
          let new_h =
            match hd with
            | Multiple (mexpr, prev_quant) ->
                let new_quant = (prev_quant * 10) + quant in
                Multiple (mexpr, new_quant)
            | _ ->
                Multiple (hd, quant)
          in
          _parse inp (n + 1) State6 (And (new_h :: tl))
      | '}' -> (
        match hd with
        | Multiple (_, _) ->
            _parse inp (n + 1) State1 expr
        (* There is no quantifier, so the previous { has to be treated as a 
         * literal. *)
        | _ ->
            _parse inp (n - 1) State1 expr
      )
      | ',' -> (
        match hd with
        | Multiple (_, _) ->
            _parse inp (n + 1) State7 expr
        (* Leading ',', has to be treated as a literal. *)
        | _ ->
            _parse inp (n - 1) State1 expr
      )
      | _ -> (
        match hd with
        | Multiple (mexpr, quant) ->
            (* We've run into a non-digit while parsing a quantifier.
             * This means the {} are literals, so we need to go back to the position
             * of the { in State1 so it can be processed as such. *)
            let num_chars = numlen quant in
            _parse inp (n - num_chars - 1) State1 (And (mexpr :: tl))
        | _ ->
            _parse inp (n - 1) State1 expr
      )
    )
  | _ ->
      failwith "internal error in state 6."

and _parse_state_7 inp n expr =
  match expr with
  | And (hd :: tl) -> (
      let current_char = inp.[n] in
      match current_char with
      | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0' ->
          let quant = int_of_char current_char - 48 in
          let new_h =
            match hd with
            | Multiple (mexpr, start_quant) ->
                BoundedRange (mexpr, start_quant, quant)
            | BoundedRange (brexpr, start_quant, prev_end_quant) ->
                let new_end_quant = (prev_end_quant * 10) + quant in
                BoundedRange (brexpr, start_quant, new_end_quant)
            | _ ->
                failwith "internal error in state 7: no BoundedRange."
          in
          _parse inp (n + 1) State7 (And (new_h :: tl))
      | '}' -> (
        match hd with
        | Multiple (mexpr, start_quant) ->
            let new_h = UnboundedRange (mexpr, start_quant) in
            _parse inp (n + 1) State1 (And (new_h :: tl))
        | BoundedRange (_, start_quant, end_quant) ->
            if start_quant <= end_quant then _parse inp (n + 1) State1 expr
            else failwith @@ out_of_range_error (n + 1)
        | _ ->
            failwith "internal error in state 7: no BoundedRange"
      )
      | _ -> (
        match hd with
        (* We can't reuse the State6 parsing because there's an additional comma. *)
        | Multiple (mexpr, quant) ->
            let num_chars = numlen quant in
            _parse inp (n - num_chars - 2) State1 (And (mexpr :: tl))
        | BoundedRange (brexpr, start_quant, end_quant) ->
            let start_chars = numlen start_quant in
            let end_chars = numlen end_quant in
            _parse inp
              (n - start_chars - end_chars - 2)
              State1
              (And (brexpr :: tl))
        | _ ->
            failwith "internal error in state 7: no BoundedRange."
      )
    )
  | _ ->
      failwith "internal error in state 7: no And list."

and _parse_state_8 inp n expr =
  match expr with
  | And (Or lst :: tl) -> (
      let current_char = inp.[n] in
      match current_char with
      | '*' | '+' | '?' ->
          failwith @@ unexpected_error current_char (n + 1)
      | '.' ->
          _parse inp (n + 1) State2 (And (Dot :: lst))
      | '[' ->
          _parse inp (n + 1) State3 (And (Or [] :: lst))
      | a ->
          _parse inp (n + 1) State1 (And (Character a :: lst))
    )
  | _ ->
      failwith "internal error in state 8"

let parse (inp : string) : expr = _parse inp 0 State1 (And [])
