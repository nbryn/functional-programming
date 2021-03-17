// Exercise 5.1
let sum first second =
    let rec go first second acc =
        match second with
        | _ when second = 0 -> (first + acc)
        | _                 -> go first (second - 1) (first + second + acc)
    
    go first second 0

// Exercise 5.2
let length lst =
    let rec go lst acc =
        match lst with
        | []      -> acc
        | _ :: xs -> go xs (acc + 1)

    go lst 0

// Exercise 5.3
let foldBack folder lst acc =
    let rec go lst acc c =
        match lst with
        | [] -> c acc
        | x :: xs -> go xs acc (fun t -> c (folder x t))

    go lst acc id

// Exercise 5.4
let factC x =
    let rec go x c =
        match x with
        | 0 -> c 1
        | _ -> go (x-1) (fun t -> c (t * x))

    go x id

// Exercise 5.5
let fibA x =
    let rec go i acc1 acc2 =
        match i with
        | _ when x = 0 -> 0
        | _ when i = x -> acc2
        | _            -> go (i+1) acc2 (acc1 + acc2) 

    go 1 0 1

let fibC x =
    let rec go i c =
        match i with
        | _ when i = 0 -> c 0
        | _ when i = 1 -> c 1
        | _            -> go (i-2) (fun r -> go (i-1) (fun vr -> c (vr + r)))

    go x id

// Exercise 5.6
// The function bigListK is not tail recursive
// because there is additional work after the recursive call.
// A solution to this would be (fun res -> c(1::res)
// now c is the last function called with no additional work afterwards

 // Exercise 5.7
type Word = (char * int) list

type aExp =
     | N of int (* Integer literal *)
     | V of string (* Variable reference *)
     | WL (* Word length *)
     | PV of aExp (* Point value lookup at word index *)
     | Add of aExp * aExp (* Addition *)
     | Sub of aExp * aExp (* Subtraction *)
     | Mul of aExp * aExp (* Multiplication *)
     | CharToInt of cExp (* NEW: Cast to integer *)

and cExp =
     | C of char (* Character literal *)
     | CV of aExp (* Character lookup at word index *)
     | ToUpper of cExp (* Convert character to upper case *)
     | ToLower of cExp (* Convert character to lower case *)
     | IntToChar of aExp (* NEW: Cast to character *)

let rec arithEvalSimple (a : aExp) (w : Word) (s : Map<string, int>) : int =   
    match a with
    | WL          -> w.Length
    | N a         -> a
    | V exp       -> Map.tryFind exp s |> fun x -> if (x = None) then 0 else x.Value
    | PV a        -> snd w.[arithEvalSimple a w s]             
    | Add(a, b)   -> arithEvalSimple a w s + arithEvalSimple b w s
    | Sub(a, b)   -> arithEvalSimple a w s - arithEvalSimple b w s
    | Mul(a, b)   -> arithEvalSimple a w s * arithEvalSimple b w s
    | CharToInt a -> charEvalSimple a w s |> int
    
and charEvalSimple (c : cExp) (w : Word) (s : Map<string, int>) : char =
    match c with
    | C a         -> a
    | CV a        -> fst w.[arithEvalSimple a w s]
    | ToUpper a   -> System.Char.ToUpper (charEvalSimple a w s) 
    | ToLower a   -> System.Char.ToLower (charEvalSimple a w s) 
    | IntToChar a -> arithEvalSimple a w s |> char

 // Exercise 5.8
let rec arithEvalTail (a : aExp) (w : Word) (s : Map<string, int>) (c : int -> 'a) : 'a =   
    match a with
    | WL          -> c w.Length
    | N a         -> c a
    | V exp       -> if Map.containsKey exp s then c (Map.find exp s) else c 0
    | PV a        -> arithEvalTail a w s (fun r -> c (snd w.[r]))            
    | Add(a, b)   -> arithEvalTail a w s (fun r -> arithEvalTail b w s (fun vr -> c (r + vr)))
    | Sub(a, b)   -> arithEvalTail a w s (fun r -> arithEvalTail b w s (fun vr -> c (r - vr)))
    | Mul(a, b)   -> arithEvalTail a w s (fun r -> arithEvalTail b w s (fun vr -> c (r * vr)))
    | CharToInt a -> charEvalTail a w s (fun r -> c (int r))

and charEvalTail (c : cExp) (w : Word) (s : Map<string, int>) (d : char -> 'a) : 'a =
    match c with
    | C a         -> d a
    | CV a        -> arithEvalTail a w s (fun r -> d (fst w.[r]))
    | ToUpper a   -> charEvalTail a w s (fun r -> d (System.Char.ToUpper r))
    | ToLower a   -> charEvalTail a w s (fun r -> d (System.Char.ToLower r))
    | IntToChar a -> arithEvalTail a w s (fun r -> d (char r))


let arithEval a w s = arithEvalTail a w s id
let charEval c w s = charEvalTail c w s id