open System

// Exercise 1.1
let sqr x = x * x

// Exercise 1.2
let pow x n = System.Math.Pow(x, n)

// Exercise 1.3
let rec sum n = 
    match n with
    | 0 -> 0
    | n -> n + sum (n-1)

// Exercise 1.4
let rec fib n =
    match n with
    | 0 -> 0
    | 1 -> 1
    | n -> fib (n-1) + fib (n-2)

// Exercise 1.5
// 1. (float * (int -> int))
// 2. int
// 3. float
// 4. (float * int -> float) * (int -> int)

// Exercise 1.6
let dup s = s^s

 // Exercise 1.7
let rec dupn s n =
 match n with
    | (n) when n <= 0 -> ""
    | _ -> s + (dupn s (n-1))
   
// Exercise 1.8
let rec bin n k =
 match (n, k) with
  | (n, 0) -> 1
  | (n, k) when n = k -> 1
  | (n, k) -> bin (n-1) (k-1) + bin (n-1) k


let rec f =
 function
 | (0,y) -> y
 | (x,y) -> f(x-1, x*y)

// Exercise 1.9
// 1. int * int -> int
// 2. Positive values of x
// 3. f(2, 3) -> f(1, 6) -> 6
// 4. f(x,y) might be called a function of two variables


// Exercise 1.10
// 1. bool * int -> int
// 2. This results in a stackoverflow, as fact -1 is evaluated immediately
// 3. This results in 0, because fact -1 is never evaluated

// Exercise 1.11
let curry f = fun g -> fun h -> f (g, h)
let uncurry f = fun (g, h) -> f g h

// Exercise 1.12
let empty (letter, pointValue : int) = function pos -> (letter, pointValue)

// Exercise 1.13
let add newPos (letter, pointValue) word = function pos ->
  if newPos = pos then (letter, pointValue)
  else word pos
  
// Exercise 1.14
let hello = add 0 ('H', 4) (empty ('A', 0)) |> add 1 ('E', 1) |> add 2 ('L', 1) |> add 3 ('L', 1) |> add 4 ('O', 1)

let hello1 pos =
 match pos with
 | (pos) when pos = 0 -> empty ('H', 4) pos
 | (pos) when pos = 1 -> empty ('E', 1) pos
 | (pos) when pos = 2 || pos = 3 -> empty ('L', 1) pos
 | (pos) when pos = 4 -> empty ('O', 1) pos
 | _ -> ('O', 0)  

// Exercise 1.15
let singleLetterScore word pos = snd (word pos)

let doubleLetterScore word pos = 2 * snd (word pos)

let tripleLetterScore word pos = 3 * snd (word pos) 

 

   
