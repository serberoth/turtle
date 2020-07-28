[<AutoOpen>]
module Ast =
    type Arg = int
    type Command =
        | Forward of Arg
        | Back of Arg
        | Right of Arg
        | Left of Arg
        | Repeat of Arg * Command list

open Ast
open System
open System.Reflection
open System.Reflection.Emit

#r "packages/FParsec.1.1.1/lib/net45/FParsecCS.dll"
#r "packages/FParsec.1.1.1/lib/net45/FParsec.dll"

module Parser =
    open Ast
    open FParsec

    let pforward = 
        (pstring "fd" <|> pstring "forward") >>. spaces1 >>. pfloat
        |>> fun x -> Forward(int x)
    let pback = 
        (pstring "bk" <|> pstring "back") >>. spaces1 >>. pfloat
        |>> fun x -> Back(int x)
    let pleft =
        (pstring "lt" <|> pstring "left") >>. spaces1 >>. pfloat
        |>> fun x -> Left(int x)
    let pright =
        (pstring "rt" <|> pstring "right") >>. spaces1 >>. pfloat
        |>> fun x -> Right(int x)

    let prepeat, prepeatimpl = createParserForwardedToRef()

    let pcommand = pforward <|> pback <|> pleft <|> pright <|> prepeat

    let block = between (pstring "[") (pstring "]") (many1 (pcommand .>> spaces))

    prepeatimpl :=
        pstring "repeat" >>. spaces1 >>. pfloat .>> spaces .>>. block
        |>> fun (n, commands) -> Repeat(int n, commands)

    let parse code =
        match run (many pcommand) code with
        | Success(result, _, _) -> result
        | Failure(msg, _, _) -> failwith msg

// #r "TurtleLibrary.dll"

let emiInstructions (il:ILGenerator) program = 
    let rec emitCommand command =
        match command with
        | Forward(n) -> emitInvoke "Forward" [|n|]
        | Back(n) -> emitInvoke "Forward" [|-n|]
        | Left(n) -> emitInvoke "Turn" [|-n|]
        | Right(n) -> emitInvoke "Turn" [|n|]
        | Repeat(0, commands) -> ()
        | Repeat(n, commands) ->
            let local = il.DeclareLocal(typeof<int>)
            // emit the loop counter
            il.Emit(OpCodes.Ldc_I4, n)
            il.Emit(OpCodes.Stloc, local.LocalIndex)
            let label = il.DefineLabel()
            il.MarkLabel(label)
            for command in commands do emitCommand command
            // declare repeat
            il.Emit(OpCodes.Ldloc, local.LocalIndex)
            il.Emit(OpCodes.Ldc_I4_1)
            il.Emit(OpCodes.Sub)
            il.Emit(OpCodes.Stloc, local.LocalIndex)
            // compare with 0
            il.Emit(OpCodes.Ldloc, local.LocalIndex)
            il.Emit(OpCodes.Ldc_I4_0)
            // repeat count > 0
            il.Emit(OpCodes.Cgt)
            il.Emit(OpCodes.Brtrue, label)
    and emitInvoke name args =
        for arg in args do il.Emit(OpCodes.Ldc_I4, arg)
        //let t = typeof<Turtle>
        //let mi = t.GetMethod(name)
        //il.EmitCall(OpCodes.Call, mi, null)
    emitInvoke "Init" [||]
    for command in program do emitCommand command
    emitInvoke "Show" [||]

let compileTo name (program:Command list) =
    let assemblyBuilder = 
        AppDomain.CurrentDomain.DefineDynamicAssembly(
            AssemblyName(name),
            AssemblyBuilderAccess.RunAndSave)
    let moduleBuilder = assemblyBuilder.DefineDynamicModule(name + ".exe")
    let typeBuilder = moduleBuilder.DefineType("Program", TypeAttributes.Public)
    let mainBuilder = typeBuilder.DefineMethod("_Main", MethodAttributes.Static ||| MethodAttributes.Public, typeof<Void>, [|typeof<string[]>|])
    let args = mainBuilder.DefineParameter(1, ParameterAttributes.None, "args")
    let il = mainBuilder.GetILGenerator()
    emiInstructions il program
    //il.Emit(OpCodes.Ret)
    assemblyBuilder.SetEntryPoint(mainBuilder)
    typeBuilder.CreateType() |> ignore
    assemblyBuilder.Save(name + ".exe")


let code = "repeat 3 [forward 2 left 10]"

let codeBlock = Parser.parse code

System.IO.Directory.SetCurrentDirectory(__SOURCE_DIRECTORY__)
do compileTo "MyProgram" codeBlock
"MyProgram.exe" |> System.Diagnostics.Process.Start
