[<AutoOpen>]
module Interpreter

#if INTERACTIVE
#load   "Ast.fsx"
#r      "System.Drawing.dll"
#r      "System.Windows.Forms.dll"
#endif

open Ast
open System
open System.Drawing
open System.Windows.Forms

type Turtle = { X:float; Y:float; A:int }

let execute commands =
    let form = new Form(Text="LOGO", Width=640, Height=480) 
    let width, height = 500, 500
    let image = new Bitmap(width, height)
    let picture = new PictureBox(Dock=DockStyle.Fill, Image=image)
    do form.Controls.Add(picture)
    let turtle = { X = float width/2.0; Y = float height/2.0; A = -90 }
    let pen = new Pen(Color.Red)

    let drawLine(x1, y1) (x2, y2) =
        use graphics = Graphics.FromImage(image)
        graphics.DrawLine(pen, int x1, int y1, int x2, int y2)

    let forward n turtle =
        let r = float turtle.A * Math.PI / 180.0
        let dx, dy = float n * cos r, float n * sin r
        let x, y = turtle.X, turtle.Y
        let x', y' = x + dx, y + dy
        drawLine(x, y) (x', y')
        { turtle with X = x'; Y = y' }

    let rec perform turtle = function
        | Forward n -> forward n turtle
        | Turn n -> { turtle with A = turtle.A + n }
        | Repeat(n, commands) ->
            let rec repeat turtle = function
                | 0 -> turtle
                | n -> repeat (performAll turtle commands) (n-1)
            repeat turtle n
    and performAll = List.fold perform
    performAll turtle commands |> ignore
    form.ShowDialog() |> ignore
