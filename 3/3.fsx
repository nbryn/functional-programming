open System

// Exercise 3.1
(* module AExpSimple

type aExp =
 | N of int
 | Add of aExp * aExp
 | Sub of aExp * aExp
 | Mul of aExp * aExp

let (.+.) a b = Add (a, b)
let (.-.) a b = Sub (a, b)
let (.*.) a b = Mul (a, b)

let rec arithEvalSimple (exp : aExp) = 
    match exp with
    | N exp -> exp
    | Add(a, b) -> arithEvalSimple a + arithEvalSimple b 
    | Sub(a, b) -> arithEvalSimple a - arithEvalSimple b
    | Mul(a, b) -> arithEvalSimple a * arithEvalSimple b  *)


// Exercise 3.2
(* module AExpState

type aExp =
 | N of int 
 | V of string
 | Add of aExp * aExp
 | Sub of aExp * aExp
 | Mul of aExp * aExp

let (.+.) a b = Add (a, b)
let (.-.) a b = Sub (a, b)
let (.*.) a b = Mul (a, b)

let rec arithEvalState (exp : aExp) = function (s : Map<string, int>) ->
    match exp with
    | N exp -> exp
    | V exp -> if (Map.exists (fun x _ -> x = exp) s) then Map.find exp s else 0
    | Add(a, b) -> arithEvalState a s + arithEvalState b s 
    | Sub(a, b) -> arithEvalState a s - arithEvalState b s
    | Mul(a, b) -> arithEvalState a s * arithEvalState b s *)


// Exercise 3.3
type aExp =
 | N of int // Integer value
 | V of string // Variable
 | WL // Length of the word
 | PV of aExp // Point value of character at specific word index
 | Add of aExp * aExp // Addition
 | Sub of aExp * aExp // Subtraction
 | Mul of aExp * aExp // Multiplication

 type Word = (char * int) list

let (.+.) a b = Add (a, b)
let (.-.) a b = Sub (a, b)
let (.*.) a b = Mul (a, b)

let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_");;
let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_");;
let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_");;
let arithDoubleWordScore = N 2 .*. V "_acc_";;
let arithTripleWordScore = N 3 .*. V "_acc_";;

let hello : Word = [('H', 4);('E', 1);('L', 1);('L', 1);('O', 1);]

let rec arithEval (a : aExp) (w : Word) = function (s : Map<string, int>) ->   
    match a with
    | WL -> w.Length
    | N a -> a
    | V exp -> Map.tryFind exp s |> fun x -> if (x = None) then 0 else x.Value
    | PV a -> snd w.[arithEval a w s]             
    | Add(a, b) -> arithEval a w s + arithEval b w s
    | Sub(a, b) -> arithEval a w s - arithEval b w s
    | Mul(a, b) -> arithEval a w s * arithEval b w s
    
// Exercise 3.4
type cExp =
 | C of char (* Character value *)
 | ToUpper of cExp (* Converts lower case to upper case character,
 non-characters unchanged *)
 | ToLower of cExp (* Converts upper case to lower case character,
 non characters unchanged *)
 | CV of aExp  (* Character lookup at word index *)

let rec charEval (c : cExp) (w : Word) = function (s : Map<string, int>) -> 
    match c with
    | C a -> a
    | CV a -> fst w.[arithEval a w s]
    | ToUpper a -> System.Char.ToUpper (charEval a w s) 
    | ToLower a -> System.Char.ToLower (charEval a w s) 

// Exercise 3.5
type bExp =
 | TT (* true *)
 | FF (* false *)

 | AEq of aExp * aExp (* numeric equality *)
 | ALt of aExp * aExp (* numeric less than *)

 | Not of bExp (* boolean not *)
 | Conj of bExp * bExp (* boolean conjunction *)
 | IsLetter of cExp (* check for letter *)
 | IsDigit of cExp (* check for digit *)

let (~~) b = Not b
let (.&&.) b1 b2 = Conj (b1, b2)
let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2) (* boolean disjunction *)

let (.=.) a b = AEq (a, b)
let (.<.) a b = ALt (a, b)
let (.<>.) a b = ~~(a .=. b) (* numeric inequality *)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b) (* numeric less than or equal to *)
let (.>=.) a b = ~~(a .<. b) (* numeric greater than or equal to
*)
let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)
   
let rec boolEval (b : bExp) (w : Word) = function (s : Map<string, int>) ->
    match b with
    | TT -> true
    | FF -> false
    | AEq(a, b) -> arithEval a w s = arithEval b w s
    | ALt(a, b) -> arithEval a w s < arithEval b w s
    | Not a -> not (boolEval a w s)
    | Conj(a, b) -> boolEval a w s && boolEval b w s
    | IsLetter a -> System.Char.IsLetter (charEval a w s)
    | IsDigit a -> System.Char.IsDigit (charEval a w s)

// Exercise 3.6 & 3.7
type stmnt =
 | Skip (* does nothing *)
 | Ass of string * aExp (* variable assignment *)
 | Seq of stmnt * stmnt (* sequential composition *)
 | ITE of bExp * stmnt * stmnt (* if-then-else statement *)
 | While of bExp * stmnt (* while statement *)

let rec evalStmnt (stm : stmnt) (w : Word) = function (s : Map<string, int>) ->
    match stm with
    | Skip -> s
    | Ass(x, a) -> Map.add x (arithEval a w s) s
    | Seq(stm1, stm2) -> evalStmnt stm1 w s |> evalStmnt stm2 w
    | ITE(guard, stm1, stm2) -> if (boolEval guard w s) then evalStmnt stm1 w s else evalStmnt stm2 w s
    | While(guard, stm) -> if (boolEval guard w s) then evalStmnt stm w s |> evalStmnt (While(guard, stm)) w else s

// Exercise 3.8
type SquareFun = Word -> int -> int -> int

let stmntToSquareFun (stm : stmnt) (w : Word) pos = function acc ->
  Map.find "_result_" (evalStmnt stm w (Map.ofList [("_pos_", pos); ("_acc_", acc)]))

let singleLetterScore = stmntToSquareFun (Ass ("_result_", arithSingleLetterScore))
let doubleLetterScore = stmntToSquareFun (Ass ("_result_", arithDoubleLetterScore))
let tripleLetterScore = stmntToSquareFun (Ass ("_result_", arithTripleLetterScore))
let doubleWordScore = stmntToSquareFun (Ass ("_result_", arithDoubleWordScore))
let tripleWordScore = stmntToSquareFun (Ass ("_result_", arithTripleWordScore))

let containsNumbers = 
    stmntToSquareFun 
        (Seq (Ass ("_result_", V "_acc_"),
            While (V "i" .<. WL,
                ITE (IsDigit (CV (V "i")), 
                    Seq (
                        Ass ("_result_", V "_result_" .*. N -1),
                        Ass ("i", WL)),
                    Ass ("i", V "i" .+. N 1)))))

// Exercise 3.9
type Square2 = (int * stmnt) list

let SLS = [(0, Ass ("_result_", arithSingleLetterScore))]
let DLS = [(0, Ass ("_result_", arithDoubleLetterScore))]
let TLS = [(0, Ass ("_result_", arithTripleLetterScore))]
let DWS = [(1, Ass ("_result_", arithDoubleWordScore))] @ SLS
let TWS = [(1, Ass ("_result_", arithTripleWordScore))] @ SLS

let calculatePoints2 (list : Square2 list) = fun (w : Word) -> 
    list |> List.mapi (fun i square -> List.map (fun (priority, stm) -> (priority, stmntToSquareFun stm w i)) square)
         |> List.fold (fun list n -> List.append n list) []
         |> List.sortBy (fst)
         |> List.map (snd)
         |> List.fold (( >> )) (id)
         |> fun x -> x 0

