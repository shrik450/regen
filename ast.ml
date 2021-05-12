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
