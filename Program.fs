// We set up a 9x9 array. Each element contains a list of numbers. If there is only a single number we know that is the one
// for that element. If there are several numbers those represent the options known to be possible at that place. If there
// are no numbers in a given square we know the setup to be impossible.
// Plan is:
// - Read in the test.sdk and stick in a 9x9 array
// - Elminate the obviously impossible, guess when that strategu runs out
// The interesting parts are in findsolution where we set up the depth-first search. 
// The rest of the file is just straightforward implementation of the sudoku game mechanics

open System
open System.IO
open System.Text
open Microsoft.FSharp.Collections

type sdkelt =
     | Certain of int32
     | Options of List<int32>
     | Nothing

// Can't do fold etc over  2d array so this flattens it
let flatten (A:'a[,]) = A |> Seq.cast<'a>

// Count how many certains are in the square
let countcertains (arr:sdkelt[,]) =
    flatten arr |> Seq.filter( fun x -> match x with
                                            | Certain i -> true
                                            | _ -> false ) |> Seq.length

// Check if we have contradictions
let iscontradiction (arr:sdkelt[,]) =
    let imps = flatten arr |> Seq.filter( fun x -> match x with
                                                    | Nothing -> true
                                                    | _ -> false ) |> Seq.length
    match imps with
        | 0 -> false
        | _ -> true

// Find the (i,j) of the element we want to guess at. Should rewrite this one I guess
let getelementtoguessat (arr:sdkelt[,]) =
    let mutable numelts = 9
    let mutable ibest = 0
    let mutable jbest = 0
    for i in 0 .. 8 do
        for j in 0 .. 8 do
            match arr.[i,j] with
                | Options opt -> if opt.Length < numelts then
                                            numelts <- opt.Length
                                            ibest <- i
                                            jbest <- j
                | _ -> ignore 0
    (ibest, jbest)

// Find a list of the values that can definitely not go at position i,j
let findimpossibles (arr:sdkelt[,]) i j =
    let rowimp = seq { for k in 0 .. 8 do 
                            if k <> i then 
                                match arr.[k , j] with
                                        | Certain x -> yield x
                                        | _ -> ignore 0
                        } |> Seq.toList
    let colimp = seq { for k in 0 .. 8 do 
                            if k <> j then 
                                match arr.[i, k] with
                                        | Certain x -> yield x
                                        | _ -> ignore 0
                        } |> Seq.toList
    let sqrimp = seq { for k in (i / 3) * 3 .. (i / 3) * 3 + 2 do
                            for l in (j / 3) * 3 .. (j / 3 ) * 3 + 2 do
                                if  not ( k = i && l = j) then 
                                    match arr.[k, l] with 
                                        | Certain x -> yield x
                                        | _ -> ignore 0
        } 
    let sqrlst = sqrimp    |> Seq.toList
    [rowimp; colimp; sqrlst ] |> List.concat

// Eliminate the obviously impossible values based on the certains we have
let rec eleminateimpossibles arr : sdkelt[,] =
    let countcertainsbefore = countcertains arr
    let res = arr |> Array2D.mapi(fun i j (el:sdkelt) -> 
            let impos = findimpossibles arr i j
            let res = match arr.[i, j] with
                        | Nothing -> sdkelt.Nothing
                        | Certain x -> if (impos |> Seq.exists(fun y -> x = y)) then sdkelt.Nothing else Certain x
                        | Options opt -> 
                                let take, leave = opt |> List.partition(fun x -> 
                                    (impos |> Seq.tryFindIndex(fun y -> x = y))  = None
                                )
                                let r = 
                                    if List.length(take) > 1 then Options take 
                                    elif List.length(take) = 1 then Certain (List.head(take))
                                    else Nothing
                                r
            res
    )
    if countcertainsbefore = (countcertains res) then res
    else eleminateimpossibles res

// Check if we are done
let issolved  (arr:sdkelt[,]) =
    (countcertains (eleminateimpossibles arr) = 9 * 9)


// Find "the" solution. Alternately elmininate obviously impossibles and guess when you can't do obvious eliminations
// I only find the first solution if there are multiple posibilites
let findsolution (arr:sdkelt[,]) =
    let f (arr:sdkelt[,]) i j =
        match arr.[i,j] with 
                | Options opt ->  opt 
                                    |> List.toArray 
                                    |> Array.Parallel.map(fun x -> 
                                                let cp = arr |> Array2D.copy
                                                cp.[i, j] <- sdkelt.Certain x
                                                eleminateimpossibles cp)
                                    |> Array.toSeq
                | Certain x -> [ arr ]  |> Seq.map(fun x -> x)
                | Nothing -> [] |> Seq.map(fun x -> x)
        |> Seq.filter(fun a -> not (iscontradiction a))
    let rec g se =
        seq {
            for x in se do
                if issolved x then yield x
                else if not (iscontradiction x) then
                    let (i, j) = getelementtoguessat x
                    let expand = f x i j
                    yield! g expand
        }
    [ (eleminateimpossibles arr) ] 
    |> List.toSeq
    |> g 
    |> Seq.head

// Print out the solution as a long string of numbers
// I use http://www.sudoku-solutions.com/ to check the results
let converttostring arr =
    let s:StringBuilder = StringBuilder()
    arr |> Array2D.iter(fun x -> Printf.bprintf s "%s" (match  x with 
                                                        | Certain i -> i.ToString()
                                                        | Options opt -> "."
                                                        | Nothing -> "-"
                                                        ))
    s.ToString()

// We keep our stuff in this. Doing a 2D array was a bad idea
let elems : sdkelt[,] = Array2D.init 9 9 (fun i j -> sdkelt.Options [])

let timer = new System.Diagnostics.Stopwatch()

File.ReadLines("test.sdk")
    |> Seq.map (fun x -> 
                    x.Trim().ToCharArray()
                    |> Array.map(fun x -> 
                                    let ci = Int32.Parse(x.ToString())
                                    if ci = 0 then sdkelt.Options [ 1 .. 9 ]
                                    else sdkelt.Certain ci
                                    )
                    |> Array.iteri(fun n x ->
                                      let i = n / 9
                                      let j = n % 9
                                      elems.[i, j] <- x
                                   )
                    elems  
               )
    |> Seq.iter (fun x ->
                    printfn "--"
                    timer.Reset()
                    timer.Start() 
                    (findsolution x) 
                    |> converttostring
                    |> printfn "%s" 
                    timer.Stop()
                    printfn "%A" timer.ElapsedMilliseconds
                )

//Pause with the solution on screen
ignore (Console.ReadLine())