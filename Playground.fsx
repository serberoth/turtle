module Playground =

    let rec y f = 
        let g x = (f (y f)) x
        g

    let fact n =
        let rec f (n, acc) =
            match n with
            | 0 -> acc
            | n -> f (n-1, acc * n)
        f(n, 1)

    printfn "Factorial: %d" (fact 10)
