type number = Float of float | Integer of int
type operator = Plus | Minus | Star | Slash
type keyword = Return | Int | Main
type symbol = LeftParen | RightParen | LeftCurly | RightCurly | Semicolon

type expression =
  | Literal of number
  | String of string
  | Binary of expression * operator * expression
