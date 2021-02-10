open System

// Exercise 2.1
let downto1 = function n ->
    if n < 0 then []
    else [n .. -1 .. 1]

let downto2 =
    function
    | n when n < 0 -> []
    | n -> [n .. -1 .. 1]

// Exercise 2.2
let removeOddIdx xs = 
    xs |> List.mapi(fun index element -> element, index)
       |> List.filter(fun (element, index) -> index % 2 = 0)
       |> List.map fst

// Exercise 2.3
let rec combinePair =
    function
    | [] -> []
    | [_] -> []
    | x::xs::xx -> (x, xs) :: combinePair xx

// Exercise 2.4
type Complex = float * float

let mkComplex first = function second -> Complex (first, second)

let complexToPair = function (complex : Complex) -> (fst complex, snd complex)


// Exercise 2.7
let explode1 = function (s : string) -> Seq.toList s

let rec explode2 = function (s : string) ->
    match s with
    | s when s.Length = 0 -> []
    | s -> s.Chars 0 :: explode2 (s.Substring 1)

// Exercise 2.8
//let implode = function (s : char list) -> List.foldBack (fun (acc : string) x  -> x + acc ) s ""

//let implode = function (s : char list) -> s |> List.map string |> List.reduce (+)

//let implodeRev = function (s : char list) -> s |> List.map string |> List.rev |> List.reduce (+)

let implode = function (s : char list) -> String.Concat(Array.ofList(s))

let implodeRev = function (s : char list) -> String.Concat(Array.ofList(List.rev s))


// Exercise 2.9