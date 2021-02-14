open System

// Exercise 2.1
let downto1 = function n ->
    if n < 0 then []
    else [n .. -1 .. 1]

let rec downto2 n =
    match n with
    | n when n <= 0 -> []
    | n -> n :: downto2 (n-1)

// Exercise 2.2
let rec removeOddIdx xs =
    match xs with
    | [] -> []
    | [x] -> [x]
    | (x :: y :: xy) -> x :: removeOddIdx xy

// Exercise 2.3
let rec combinePair =
    function
    | [] -> []
    | [_] -> []
    | x::xs::xx -> (x, xs) :: combinePair xx

// Exercise 2.4
type Complex = float * float

let mkComplex first = function second -> Complex (first, second)

let complexToPair = function a -> (fst a, snd a)

let (|*|) (a : Complex) (b : Complex) = Complex ((fst a * fst b) - (snd a * snd b), (snd a * fst b) + (fst a * snd b))

let (|+|) (a : Complex) (b : Complex) = Complex (fst a + fst b, snd a + snd b)

let (|-|) (a : Complex) (b : Complex) = Complex (fst a - fst b, snd a - snd b)

let (|/|) (a : Complex) (b : Complex) = 
    let first = (fst a * fst b + snd a * snd b) / (Math.Pow(fst b, 2.) + Math.Pow(snd b, 2.))
    let second= (snd a * fst b - fst a * snd b) / (Math.Pow(fst b, 2.) + Math.Pow(snd b, 2.))
    Complex (first, second)

// Exercise 2.7
let explode1 = function (s : string) -> Seq.toList s

let rec explode2 = function (s : string) ->
    match s with
    | s when s.Length = 0 -> []
    | s -> s.Chars 0 :: explode2 (s.Substring 1)

// Exercise 2.8
let implode = function (s : char list) -> String.Concat(Array.ofList(s))
let implodeRev = function (s : char list) -> String.Concat(Array.ofList(List.rev s))

let implode2 = function (s : char list) -> List.foldBack (fun x acc  -> string x + acc ) s ""
let implodeRev2 (arr : char list) = List.fold (fun acc x -> string x + acc) "" arr


// Exercise 2.9
let toUpper s = s |> explode1 |> List.map Char.ToUpper |> implode

// Exercise 2.10
let rec ack =
    function
    | (m, n) when m = 0 -> n+1
    | (m, n) when m > 0 && n = 0 -> ack (m-1, 1)
    | (m, n) when m > 0 && n > 0 -> ack (m-1, ack (m, n-1))
    | _ -> failwithf "Invalid Arg"

// Exercise 2.11
let timeArg1 f = fun a ->
 let start = System.DateTime.Now
 let res = f (a)
 let finish = System.DateTime.Now
 sprintf "%d mins ago" (int (finish - start).TotalMinutes)
 (res, finish - start)

// Exercise 2.12
let downto32 f n e =
        if n > 0 then
            List.foldBack f [1..n] e
        else e

let rec downto3 f n e =
        if n > 0 then
            f n e |> downto3 f (n-1)
        else e

let fac n = downto3 (*) n 1

let range g n = downto3 (fun x y -> (g x) :: y) n []

// Exercise 2.13
type word = (char * int) list

type squareFun = word -> int -> int -> int

type square = (int * squareFun) list

let hello : word = [('H', 4);('E', 1);('L', 1);('L', 1);('O', 1);]

let suppe : word = [('S', 2);('U', 3);('P', 2);('P', 2);('E', 1);]

// Exercise 2.14
let singleLetterScore (word : word) pos = fun acc -> snd (word.[pos]) + acc 

let doubleLetterScore (word : word) pos = fun acc -> 2 * snd (word.[pos]) + acc 

let tripleLetterScore (word : word) pos = fun acc -> 3 * snd (word.[pos]) + acc 

// Exercise 2.15
let doubleWordScore : squareFun = fun word pos acc -> 2 * acc

let tripleWordScore : squareFun = fun word pos acc -> 3 * acc

// Exercise 2.16
let containsNumbers : squareFun = fun word pos acc ->
  if (word |> List.exists (fun x -> Char.IsNumber(fst x))) 
  then -(acc)
  else acc

// Exercise 2.17
let SLS : square = [(0, singleLetterScore)]
let DLS : square = [(0, doubleLetterScore)]
let TLS : square = [(0, tripleLetterScore)]
let DWS : square = SLS @ [(1, doubleWordScore)]
let TWS : square = SLS @ [(1, tripleWordScore)]

let calculatePoints (list : square list) = fun (word : word) -> 
    list |> List.mapi (fun i square -> List.map (fun (prority, squareFun) -> (prority, squareFun word i) ) square)
         |> List.fold (fun list n -> List.append n list) []
         |> List.sortBy (fun x -> fst x)
         |> List.map (fun x -> snd x)
         |> List.fold (fun f n -> f >> n) (fun x -> x)
         |> fun x -> x 0