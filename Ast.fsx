[<AutoOpen>]
module Ast

type Arg = int
type Command =
    | Forward of Arg
    | Turn of Arg
    | Repeat of Arg * Command list
