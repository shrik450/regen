open Ast

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

type governor_state =
  (* Base State; posting to the base And list. *)
  | GState1
  (* State when dealing with |, posting to the head Or list. *)
  | GState2
  (* State for dealing with subexpressions, posting to the head And list. *)
  | GState3

let unexpected_error = Printf.sprintf "Parse failure: Unexpected `%c' at col %d"

let out_of_range_error =
  Printf.sprintf "Parse failure: Invalid range at col %d."

let numlen n = n |> string_of_int |> String.length

let rec _parse_one_expr inp n state expr =
  let len = String.length inp in
  if len = n then
    match state with
    (* While State2 is expecting a quantifier, a lack of a quantifier never
     * hurt anyone. *)
    | State1 | State2 ->
        (expr, n)
    | State3 | State4 | State5 ->
        failwith "Unexpected EOF, expecting `]'."
    (* TODO: Implement conversion to literal if this happens. *)
    | State6 | State7 ->
        failwith "Unexpected EOF, expecting `}'."
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

and _parse_state_1 inp n expr =
  let current_char = inp.[n] in
  match current_char with
  | '*' | '+' | '?' ->
      failwith @@ unexpected_error current_char (n + 1)
  | '.' ->
      _parse_one_expr inp (n + 1) State2 Dot
  | '[' ->
      _parse_one_expr inp (n + 1) State3 (Or [])
  | a ->
      _parse_one_expr inp (n + 1) State2 (Character a)

and _parse_state_2 inp n expr =
  let current_char = inp.[n] in
  match current_char with
  | '?' ->
      let new_expr = BoundedRange (expr, 0, 1) in
      (new_expr, n + 1)
  | '*' ->
      let new_expr = UnboundedRange (expr, 0) in
      (new_expr, n + 1)
  | '+' ->
      let new_expr = UnboundedRange (expr, 1) in
      (new_expr, n + 1)
  | '{' ->
      _parse_one_expr inp (n + 1) State6 expr
  | _ ->
      (expr, n)

and _parse_state_3 inp n expr =
  let current_char = inp.[n] in
  match current_char with
  | ']' ->
      _parse_one_expr inp (n + 1) State2 expr
  (* A leading - is also treated as a literal. *)
  | a -> (
    match expr with
    | Or lst ->
        let new_expr = Or (Character a :: lst) in
        _parse_one_expr inp (n + 1) State4 new_expr
    | _ ->
        failwith "internal failure in state 3: no OR list."
  )

and _parse_state_4 inp n expr =
  let current_char = inp.[n] in
  match current_char with
  | '-' ->
      _parse_one_expr inp (n + 1) State5 expr
  | _ ->
      _parse_one_expr inp n State3 expr

and _parse_state_5 inp n expr =
  match expr with
  | Or (Character c :: tl1) -> (
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
                @@ Character (char_of_int (init + n)) :: lst
            in
            (* Note that [add_chars] adds characters in "forward" order - i.e., a-c is
             * added as [Character a; Character b; Character c]. This is important
             * because in all other cases, we add them in reverse order as we
             * progress through the input string. How the expressions are ordereed
             * matters in an And list, since we need to output a string that matches
             * the ordering of the input regex. However, in this case it doesn't make
             * a difference, since the output is an Or expression. *)
            let new_expr = Or (add_chars init diff tl1) in
            _parse_one_expr inp (n + 1) State3 new_expr
    )
  | _ ->
      failwith "internal failure in state 5: no OR list."

and _parse_state_6 inp n expr =
  let current_char = inp.[n] in
  match current_char with
  | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0' ->
      let quant = int_of_char current_char - 48 (* 48 is ascii for 0. *) in
      let new_expr =
        match expr with
        | Multiple (mexpr, prev_quant) ->
            let new_quant = (prev_quant * 10) + quant in
            Multiple (mexpr, new_quant)
        | _ ->
            Multiple (expr, quant)
      in
      _parse_one_expr inp (n + 1) State6 new_expr
  | '}' -> (
    match expr with
    | Multiple (_, _) ->
        (expr, n + 1)
    (* There is no quantifier, so the previous { has to be treated as a
     * literal. *)
    | _ ->
        (expr, n - 1)
  )
  | ',' -> (
    match expr with
    | Multiple (_, _) ->
        _parse_one_expr inp (n + 1) State7 expr
    (* A leading ',', has to be treated as a literal. *)
    | _ ->
        (expr, n - 1)
  )
  | _ -> (
    match expr with
    | Multiple (mexpr, quant) ->
        (* We've run into a non-digit while parsing a quantifier.
         * This means the {} are literals, so we need to go back to the position
         * of the { in State1 so it can be processed as such. *)
        let num_chars = numlen quant in
        (mexpr, n - num_chars - 1)
    | _ ->
        (expr, n - 1)
  )

and _parse_state_7 inp n expr =
  let current_char = inp.[n] in
  match current_char with
  | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0' ->
      let quant = int_of_char current_char - 48 in
      let new_expr =
        match expr with
        | Multiple (mexpr, start_quant) ->
            BoundedRange (mexpr, start_quant, quant)
        | BoundedRange (brexpr, start_quant, prev_end_quant) ->
            let new_end_quant = (prev_end_quant * 10) + quant in
            BoundedRange (brexpr, start_quant, new_end_quant)
        | _ ->
            failwith "internal error in state 7: no BoundedRange."
      in
      _parse_one_expr inp (n + 1) State7 new_expr
  | '}' -> (
    match expr with
    | Multiple (mexpr, start_quant) ->
        let new_expr = UnboundedRange (mexpr, start_quant) in
        (new_expr, n + 1)
    | BoundedRange (_, start_quant, end_quant) ->
        if start_quant <= end_quant then (expr, n + 1)
        else failwith @@ out_of_range_error (n + 1)
    | _ ->
        failwith "internal error in state 7: no BoundedRange"
  )
  | _ -> (
    match expr with
    (* We can't reuse the State6 parsing because there's an additional comma. *)
    | Multiple (mexpr, quant) ->
        let num_chars = numlen quant in
        (mexpr, n - num_chars - 2)
    | BoundedRange (brexpr, start_quant, end_quant) ->
        let start_chars = numlen start_quant in
        let end_chars = numlen end_quant in
        (brexpr, n - start_chars - end_chars - 2)
    | _ ->
        failwith "internal error in state 7: no BoundedRange."
  )

let rec _parse inp n state expr =
  let len = String.length inp in
  if n >= len then expr
  else
    let current_char = inp.[n] in
    match state with
    | GState1 -> (
      match expr with
      | And lst -> (
        match current_char with
        | '|' ->
            _parse inp (n + 1) GState2 expr
        | _ ->
            let next_expr, next_n = _parse_one_expr inp n State1 Empty in
            _parse inp next_n GState1 (And (next_expr :: lst))
      )
      | _ ->
          failwith "internal error in GState1: no AND list."
    )
    | GState2 -> (
      match current_char with
      | '|' ->
          failwith @@ unexpected_error '|' (n + 1)
      | _ -> (
        match expr with
        | And (h :: t) -> (
          match h with
          | Or lst ->
              let next_expr, next_n = _parse_one_expr inp n State1 Empty in
              _parse inp next_n GState1 (And (Or (next_expr :: lst) :: t))
          | _ ->
              _parse inp n GState2 (And (Or [h] :: t))
        )
        | _ ->
            failwith "internal error in GState2: no AND list"
      )
    )
    | _ ->
        failwith "Not implemented."

let parse (inp : string) : expr = _parse inp 0 GState1 (And [])
