#r "FParsecCS.dll"
#r "FParsec.dll"
#load "Ast.fsx"
#load "Interpreter.fsx"

open Ast
open Interpreter

open FParsec

let test p str =
    match run p str with
    | Success(result, _, _)     -> printfn "Success: %A" Result
    | Failure(errorMsg, _, _)   -> printfn "Failure: %s" errorMsg

let forward = pstring "forward" >>. spaces1 >>. pfloat
test forward "forward 10"

let pfwd = forward |>> fun n -> Forward(int n)
test pfwd "forward 10"

let pforward = (pstring "fd" <|> pstring "forward") >>. spaces1 >>. pfloat
test pforward "forward 10"
test pforward "fd 10"

module Parser =
    open Ast
    open FParsec

    let pforward = 
        (pstring "fd" <|> pstring "forward") >>. spaces1 >>. pfloat
        |>> fun x -> Forward(int x)
    let pleft =
        (pstring "lt" <|> pstring "left") >>. spaces1 >>. pfloat
        |>> fun x -> Left(int x)
    let pright =
        (pstring "rt" <|> pstring "right") >>. spaces1 >>. pfloat
        |>> fun x -> Right(int x)

    let prepeat, prepateimpl = createParserForwardedToRef()

    let pcommand = pforward <|> pleft <|> pright <|> prepeat

    let block = between (pstring "[") (pstring "]") (many1 (pcommand .>> spaces))

    prepeatimpl :=
        pstring "repeat" >>. spaces1 >>. pfloat .>> spaces .>>. block
        |>> fun (n, commands) -> Repeat(int n, commands)

    let parse code =
        match run (many pcommand) code with
        | Success(result, _, _) -> result
        | Failure(msg, _, _) -> failwith msg

let code = "repeat 10 [right 36 repeat 5 [forward 54 right 72]]"
let program = Parser.parse code
Interpreter.execute program
