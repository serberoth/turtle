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

    let procs = ref Map.empty

    let padd(n, a, b) = procs := Map.add n (a, b) !procs

    padd("a", 1, 2)
    padd("b", 3, 4)
    
    let data = [("Cats",4);
                ("Dogs",5);
                ("Mice",3);
                ("Elephants",8)]
    let count = data |> List.fold (fun acc (n, x) -> acc + x) 0
    //let count = List.fold (fun acc (nm,x) -> acc+x) 0 data
    printfn "Total number of animals: %d" count
