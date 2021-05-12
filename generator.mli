(* [generate regexp] is a string matching the regular expression [regexp].
 * The regular expression parser only implements a subset of common regexp
 * operations.
 * For practical reasons, unbounded operations like [*] are limited to 255
 * characters. *)
val generate : Ast.expr -> string
