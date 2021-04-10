module ImpParser

    open Eval

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    open JParsec.TextParser             // Example parser combinator library. Use for CodeJudge.
    // open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy (fun x -> System.Char.IsWhiteSpace x)
    let pletter        = satisfy (fun x -> System.Char.IsLetter x)
    let palphanumeric  = satisfy (fun x -> System.Char.IsLetterOrDigit x)

    let spaces         = many (pchar ' ')
    let spaces1        = many1 (pchar ' ')

    let (.>*>.) (p1 : Parser<'a>) (p2 : Parser<'b>) = p1 .>> spaces .>>. p2
    let (.>*>) (p1 : Parser<'a>) (p2 : Parser<'b>)  = p1 .>> spaces .>> p2
    let (>*>.) (p1 : Parser<'a>) (p2 : Parser<'b>)  = p1 .>> spaces >>. p2

    let parenthesise (p : Parser<'a>) = (((pstring "(" >>. spaces) >>. p) .>> spaces) .>> pstring ")"

    let convertToString (l : char list) = List.ofSeq l |> List.toArray |> System.String

    let pid = (pletter <|> pchar ('_')) .>>. (many palphanumeric .>>. many (pchar '_')) |>> fun (x, (y, z)) -> convertToString (x::y@z)

    
    let unop op a = (op .>> spaces) >>. a
    let binop op p1 p2 = (((p1 .>> spaces) .>> op) .>> spaces) .>>. p2

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()

    let TParse, href = createParserForwardedToRef<cExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    let NParse         = pint32 |>> N <?> "Int"
    let VParse         = pid |>> V <?> "String"
    let PVParse        = unop (pstring "pointValue") AtomParse |>> PV <?> "PV"
    let NegParse       = pchar '-' >>. pint32 |>> fun x -> Mul (N -1, N x)
    let CharToIntParse = pstring "charToInt" .>>. spaces >>. parenthesise TParse |>> CharToInt <?> "CharToInt"
    let ParParse       = parenthesise TermParse
    do aref := choice [CharToIntParse; NegParse; PVParse;  VParse; NParse; ParParse]

    let AexpParse = TermParse

    let CParse         = pchar ''' >>. (pletter <|> pchar ' ') .>> pchar ''' |>> C <?> "C"
    let ToLowerParse   = pstring "toLower" .>>. spaces >>. parenthesise TParse |>> ToLower <?> "ToLower"
    let ToUpperParse   = pstring "toUpper" .>>. spaces >>. parenthesise TParse  |>> ToUpper <?> "ToUpper"
    let IntToCharParse = pstring "intToChar" .>>. spaces >>. parenthesise AtomParse |>> IntToChar <?> "IntToChar"
    let CVParse        = unop (pstring "charValue") AtomParse |>> CV <?> "CV"
    let PrParse        = parenthesise TParse
    do href := choice [IntToCharParse; ToLowerParse; ToUpperParse; CVParse; CParse; PrParse]

    let CexpParse = TParse

    let BexpParse = pstring "not implemented"

    let stmntParse = pstring "not implemented"

(* These five types will move out of this file once you start working on the project *)
    type coord      = int * int
    type squareProg = Map<int, string>
    type boardProg  = {
            prog       : string;
            squares    : Map<int, squareProg>
            usedSquare : int
            center     : coord
    
            isInfinite : bool   // For pretty-printing purposes only
            ppSquare   : string // For pretty-printing purposes only
        }

    type word   = (char * int) list
    type square = Map<int, word -> int -> int -> int>

    let parseSquareFun _ = failwith "not implemented"

    let parseBoardFun _ = failwith "not implemented"

    type boardFun = coord -> square option
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun
    }

    let parseBoardProg (bp : boardProg) = failwith "not implemented"

