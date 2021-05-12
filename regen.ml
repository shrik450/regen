let generate str = str |> Parser.parse |> Generator.generate

let _ =
  if Array.length Sys.argv == 1 then
    print_endline "No arguments given. Try --help for help."
  else
    let first_arg = Sys.argv.(1) in
    match first_arg with
    | "--help" | "-h" ->
        print_endline "Under construction."
    | _ ->
        let n =
          if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 1
        in
        for i = 0 to n - 1 do
          first_arg |> generate |> print_endline
        done
