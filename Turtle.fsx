#load "Ast.fsx"
#load "Interpreter.fsx"

(* repeat 36 [right 36 turn 10] *)
execute [Repeat(36, [Forward 10; Turn 10])]

(*
repeat 10
    [turn 36 repeat 5
        [forward 54 turn 72]]
 *)
execute [Repeat(10, [Turn 36; Repeat(5, [Forward 54; Turn 72])])]
