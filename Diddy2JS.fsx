[<AutoOpen>]
module Ast =
    type Name = string
    type Identifier = string
    type PenColour =
        | Red
        | Green
        | Blue
    type PenCap = Butt | Round | Square
    type ArithmeticOp = Add | Sub | Mul | Div
    type ComparisonOp = Eq | Ne | Lt | Gt | Le | Ge
    type LogicalOp = And | Or
    type Value =
        | Bool of bool
        | Integer of int
        | Double of float
        | String of string
    type Expr =
        | Literal of Value
        | Arg of Identifier
        //| Var of Name
        | Arithmetic of Expr * ArithmeticOp * Expr
        | Comparison of Expr * ComparisonOp * Expr
        | Logical of Expr * LogicalOp * Expr
    type Command =
        | Forward of Expr
        | Back of Expr
        | Left of Expr
        | Right of Expr
        | SetRandomPosition
        | ClearScreen
        | SetPenColour of PenColour
        | SetPenSize of PenCap * Expr
        | Repeat of Expr * Command list
        | Call of Name * Expr list
        | Proc of Name * Identifier list * Command list
        | Make of Name * Expr
        | If of Expr * Command list
        | Stop

#if INTERACTIVE
#r "packages/FParsec.1.1.1/lib/net45/FParsecCS.dll"
#r "packages/FParsec.1.1.1/lib/net45/FParsec.dll"
#endif

[<AutoOpen>]
module Parser =
    open Ast
    open FParsec

    let procs = ref []

    let ws = spaces
    let ws1 = spaces1
    let pstr_ws s = pstring s .>> ws
    let pstr_ws1 s = pstring s .>> ws1

    let pidentifier =
        let isIdentifierFirstChar c = isLetter c || c = '-'
        let isIdentifierChar c = isLetter c || isDigit c || c = '-'
        many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"

    let pparam = pstring ":" >>. pidentifier

    let pboolliteral = 
        let ptrue = pstring "true" |>> fun _ -> Literal(Bool(true))
        let pfalse = pstring "false" |>> fun _ -> Literal(Bool(false))
        ptrue <|> pfalse

    let pnumliteral: Parser<Expr, unit> =
        let numberFormat = NumberLiteralOptions.AllowFraction //|| NumberLiteralOptions.AllowExponent
        numberLiteral numberFormat "number"
        |>> fun nl ->
            if nl.IsInteger then Literal(Integer(int nl.String))
            else Literal(Double(float nl.String))

    let pstrliteral =
        between (pstring "\"") (pstring "\"") (manySatisfy (fun x -> x <> '"'))
        |>> (fun s -> Literal(String(s)))

    let pliteral = pboolliteral <|> pnumliteral <|> pstrliteral

    let pprimary = pliteral <|> (pparam |>> fun a -> Arg(a))

    let termp = new OperatorPrecedenceParser<Expr, unit, unit>()
    let pterm = termp.ExpressionParser
    let term = (pprimary .>> ws) <|> between (pstr_ws "(") (pstr_ws ")") pterm 
    termp.TermParser <- term

    let termc = new OperatorPrecedenceParser<Expr, unit, unit>()
    let pconditional = termc.ExpressionParser
    termc.TermParser <- term

    let aops = ["+",Add; "-",Sub; "*",Mul; "/",Div]
    for s,op in aops do
        termp.AddOperator(InfixOperator(s, ws, 1, Associativity.Left, fun x y -> Arithmetic(x, op, y)))

    let cops = ["=",Eq; "!=",Ne; "<",Lt; ">",Gt; "<=",Le; ">=",Ge]
    for s,op in cops do
        termp.AddOperator(InfixOperator(s, ws, 1, Associativity.Left, fun x y -> Comparison(x, op, y)))
        termc.AddOperator(InfixOperator(s, ws, 1, Associativity.Left, fun x y -> Comparison(x, op, y)))

    let lops = ["and",And; "or",Or]
    for s,op in lops do
        termp.AddOperator(InfixOperator(s, ws, 1, Associativity.Left, fun x y -> Logical(x, op, y)))
        termc.AddOperator(InfixOperator(s, ws, 1, Associativity.Left, fun x y -> Logical(x, op, y)))

    let pexpr = pterm

    let pforward = 
        (pstr_ws1 "fd" <|> pstr_ws1 "forward") >>. pexpr
        |>> fun arg -> Forward(arg)
    let pbackward =
        (pstr_ws1 "bk" <|> pstr_ws1 "back") >>. pexpr
        |>> fun arg -> Back(arg)
    let pleft =
        (pstr_ws1 "lt" <|> pstr_ws1 "left") >>. pexpr
        |>> fun arg -> Left(arg)
    let pright =
        (pstr_ws1 "rt" <|> pstr_ws1 "right") >>. pexpr
        |>> fun arg -> Right(arg)
    let prandom =
        pstring "set-random-position"
        |>> fun _ -> SetRandomPosition
    let pclear = 
        pstring "clear-screen"
        |>> fun _ -> ClearScreen

    let psetpencolour : Parser<Command, unit> = 
        let pred = pstring "red" |>> fun _ -> Red
        let pgreen = pstring "green" |>> fun _ -> Green
        let pblue = pstring "blue" |>> fun _ -> Blue
        let ppencolour = pred <|> pgreen <|> pblue
        pstr_ws1 "set-pen-colour" >>. ppencolour
        |>> fun c -> SetPenColour(c)

    let psetpensize = 
        let pbutt = pstr_ws "butt" |>> fun _ -> Butt
        let pround = pstr_ws "round" |>> fun _ -> Round
        let psquare = pstr_ws "square" |>> fun _ -> Square
        let ppencap = pbutt <|> pround <|> psquare
        pstr_ws "set-pen-size" >>. ppencap .>>. pexpr
        |>> fun c -> SetPenSize(c)

    let pstop = pstr_ws "stop" |>> fun _ -> Stop

    let pmake =
        let lhs = pstr_ws "make" >>. pidentifier .>> spaces
        let rhs = pstr_ws "=" >>. pterm
        pipe2 lhs rhs (fun n t -> Make(n, t))

    let pif, pifimpl = createParserForwardedToRef()
    let prepeat, prepeatimpl = createParserForwardedToRef()
    let pcall, pcallimpl = createParserForwardedToRef()

    let pcommand = pforward <|> pbackward <|> pleft <|> pright <|> prandom <|> pclear <|> psetpencolour <|> psetpensize <|> prepeat <|> pcall <|> pmake <|> pif <|> pstop

    let updateCalls () =
        let parg = pidentifier .>> spaces |>> fun a -> Arg(a)
        pcallimpl :=
            choice [
                for name, ps in !procs ->
                    pstr_ws name >>. (many parg)
                    |>> fun args -> Call(name, args)
            ]
    updateCalls()

    let block = between (pstr_ws "[") (pstring "]") (sepEndBy pcommand ws1)

    let pifheader = pstr_ws "if" >>. pconditional .>> spaces
    pifimpl :=
        pipe2 pifheader block (fun c b -> If(c, b))

    prepeatimpl :=
        pstr_ws1 "repeat" >>. pexpr .>> spaces .>>. block
        |>> fun (arg, commands) -> Repeat(arg, commands)

    let pproc =
        let pparams = many (pparam .>> spaces)
        let pheader = pstr_ws "to" >>. pidentifier .>> ws1 .>>. pparams
        let pbody = many (pcommand .>> ws1)
        let pfooter = pstr_ws "end"
        pheader .>>. pbody .>> pfooter
        |>> fun ((name, ps), body) ->
            procs := (name, ps)::!procs; updateCalls()
            Proc(name, ps, body)

    let parser = spaces >>. (sepEndBy (pcommand <|> pproc) ws1)

    let parse code =
        match run parser code with
        | Success(result, _, _) -> result
        | Failure(msg, _, _) -> failwith msg

[<AutoOpen>]
module JSTranslator =
    open Ast
    open System

    let getVar =
        let varId = ref 0
        fun () -> incr varId; sprintf "_%d" !varId
    let rec emitBlock indent commands = 
        String.concat "" [|for command in commands -> emitCommand indent command|]
    and emitCommand indent command = 
        let tabs = String.replicate indent "\t"
        match command with
        | Forward(e) -> sprintf "forward(%s);" (emitExpr e)
        | Back(e) -> sprintf "back(%s);" (emitExpr e)
        | Right(e) -> sprintf "right(%s);" (emitExpr e)
        | Left(e) -> sprintf "left(%s);" (emitExpr e)
        | SetPenColour(Red) -> sprintf "set_pen_colour('#ff0000');"
        | SetPenColour(Green) -> sprintf "set_pen_colour('#00ff00');"
        | SetPenColour(Blue) -> sprintf "set_pen_colour('#0000ff');"
        | SetPenSize(Butt, e) -> sprintf "set_pen_size('butt', %s);" (emitExpr e)
        | SetPenSize(Round, e) -> sprintf "set_pen_size('round', %s);" (emitExpr e)
        | SetPenSize(Square, e) -> sprintf "set_pen_size('square', %s);" (emitExpr e)
        | Repeat(e, commands) ->
            let block = emitBlock (indent+1) commands
            String.Format("for({0}=0;{0}<{1};{0}++) {{\r\n {2}}", getVar(), emitExpr e, block)
        | Make(name, e) -> sprintf "let %s = %s;" name (emitExpr e)
        | If(Comparison(lhs, op, rhs), commands) ->
            let condition = sprintf "%s%s%s" (emitExpr lhs) (fromComparison op) (emitExpr rhs)
            sprintf "if(%s) {\r\n%s%s}" condition (emitBlock (indent+1) commands) tabs
        | If(Logical(lhs, op, rhs), commands) ->
            let condition = sprintf "%s%s%s" (emitExpr lhs) (fromLogical op) (emitExpr rhs)
            sprintf "if(%s) {\r\n%s%s}" condition (emitBlock (indent+1) commands) tabs
        | Stop -> sprintf "return;"
        | Proc(name, ``params``, commands) ->
            sprintf "\r\nfunction %s(%s) {\r\n%s%s}"
                name
                (String.concat "," ``params``)
                (emitBlock (indent+1) commands)
                tabs
        | Call(name, args) ->
            sprintf "%s(%s);" name (String.concat "," [for arg in args -> emitExpr arg])
        | _ -> failwith "Not implemented"
        |> fun s -> tabs + s + "\r\n"
    and emitExpr expr =
        match expr with
        | Literal(Bool(b)) -> String.Format("{0}", b)
        | Literal(Double(n)) -> String.Format("{0}", n)
        | Literal(Integer(n)) -> String.Format("{0}", n)
        | Literal(String(s)) -> sprintf "\"%s\"" s
        | Arg(s) -> s
        //| Var(s) -> s
        | Arithmetic(lhs, op, rhs) -> sprintf "%s%s%s" (emitExpr lhs) (fromArithmetic op) (emitExpr rhs)
        | Comparison(lhs, op, rhs) -> sprintf "%s%s%s" (emitExpr lhs) (fromComparison op) (emitExpr rhs)
        | Logical(lhs, op, rhs) -> sprintf "%s%s%s" (emitExpr lhs) (fromLogical op) (emitExpr rhs)
    and fromArithmetic op =
        match op with
        | Add -> "+"
        | Sub -> "-"
        | Mul -> "*"
        | Div -> "/"
    and fromComparison op =
        match op with
        | Eq -> "=="
        | Ne -> "!="
        | Lt -> "<"
        | Gt -> ">"
        | Le -> "<="
        | Ge -> ">="
    and fromLogical op =
        match op with
        | And -> "&&"
        | Or -> "||"



open Ast

let code = "
to tree :depth :length :scale :angle
    if :depth = 0 [stop]
    set-pen-size round :depth/2
    forward :length
    right :angle
    tree :depth-1 :length*:scale :scale :angle
    left 2*:angle
    tree :depth-1 :length*:scale :scale :angle
    right :angle
    back :length
end

clear-screen
tree 10 80 0.7 30
"

let program = Parser.parse code

(*
let program = [
    Proc("tree", ["depth"; "length"; "scale"; "angle"],
        [
            If(Condition(Arg("depth"), Eq, Literal(Double(0.0))), [Stop])
            Forward(Arg("length"))
            Right(Arg("angle"))
            Call("tree", [Arithmetic(Arg("depth"), Sub, Literal(Double(1.0)));
                          Arithmetic(Arg("length"), Mul, Arg("scale"));
                          Arg("scale");
                          Arg("angle")])
            Left(Arithmetic(Literal(Double(2.0)), Mul, Arg("angle")))
            Pen(Green)
            Call("tree", [Arithmetic(Arg("depth"), Sub, Literal(Double(1.0)));
                          Arithmetic(Arg("length"), Mul, Arg("scale"));
                          Arg("scale");
                          Arg("angle")])
            Pen(Blue)
            Right(Arg("angle"))
            Back(Arg("length"))
        ])
    Call("tree", [Literal(Double(10.0)); Literal(Double(80.0)); Literal(Double(0.7)); Literal(Double(30.0))])
]
 *)

JSTranslator.emitBlock 0 program

(*
open System.IO
let path = Path.Combine(__SOURCE_DIRECTORY__, "Turtle.html")
path |> System.Diagnostics.Process.Start
 *)
