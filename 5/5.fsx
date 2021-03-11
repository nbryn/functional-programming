// Exercise 5.1
let sum first second =
    let rec go first second acc =
        match second with
        | _ when second = 0 -> acc
        | _                 -> go first (second - 1) (first + second)
    
    go first second 0

// Exercise 5.2
let length lst =
    let rec go lst acc =
        match lst with
        | []      -> acc
        | _ :: xs -> go xs (acc + 1)

    go lst 0

// Exercise 5.3
let foldback folder lst acc =
    let rec go lst acc c =
        match lst with
        | [] -> c acc
        | x :: xs -> go xs acc (fun t -> c (folder x t))

    go lst acc id

// Exercise 5.4
//Define an iterative function factC : int -> int that uses continuations and compare the
//running time between the two. Which solution is faster and why?
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
        | _ when i = x -> acc2
        | _            -> go (i+1) acc2 (acc1 + acc2) 

    go 1 0 1

let fibC x =
    let rec go i c =
        match i with
        | _ when i = x -> c 1
        | _            -> go (i+1) (fun r -> c (r + r)) 

    go 1 (fun x -> x + x)

// Exercise 5.6