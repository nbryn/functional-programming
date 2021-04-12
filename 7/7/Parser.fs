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
    let PVParse        = unop pPointValue AtomParse |>> PV <?> "PV"
    let NegParse       = pchar '-' >>. pint32 |>> fun x -> Mul (N -1, N x)
    let CharToIntParse = pCharToInt .>>. spaces >>. parenthesise TParse |>> CharToInt <?> "CharToInt"
    let ParParse       = parenthesise TermParse
    do aref := choice [CharToIntParse; NegParse; PVParse; VParse; NParse; ParParse]

    let AexpParse = TermParse

    let CParse         = pchar ''' >>. (pletter <|> pchar ' ') .>> pchar ''' |>> C <?> "C"
    let ToLowerParse   = pToLower .>>. spaces >>. parenthesise TParse |>> ToLower <?> "ToLower"
    let ToUpperParse   = pToUpper .>>. spaces >>. parenthesise TParse  |>> ToUpper <?> "ToUpper"
    let IntToCharParse = pIntToChar .>>. spaces >>. parenthesise AtomParse |>> IntToChar <?> "IntToChar"
    let CVParse        = unop (pCharValue) AtomParse |>> CV <?> "CV"
    let PrParse        = parenthesise TParse
    do href := choice [IntToCharParse; ToLowerParse; ToUpperParse; CVParse; CParse; PrParse]

    let CexpParse = TParse

    let BTermParse, btref = createParserForwardedToRef<bExp>()
    let BProdParse, bpref = createParserForwardedToRef<bExp>()

    let ConParse = binop (pstring "/\\") BProdParse BTermParse |>> Conj <?> "Conj"
    let DisParse = binop (pstring "\\/") BProdParse BTermParse |>> fun (x, y) -> Not (Conj (Not x, Not y))
    do btref := choice [ConParse; DisParse; BProdParse]

    let PrrParse            = parenthesise BTermParse
    let NotParse            = unop (pchar '~') BTermParse |>> Not <?> "Not"
    let TrueParse           = pTrue |>> fun _ -> TT
    let FalseParse          = pFalse |>> fun _ -> FF
    let EqualParse          = binop (pchar '=') AtomParse AtomParse |>> AEq <?> "AEq"
    let NotEqualParse       = binop (pstring "<>") AtomParse AtomParse |>> fun x -> Not (AEq x)
    let LessParse           = binop (pchar '<') AtomParse AtomParse |>> ALt <?> "ALt"
    let GreaterParse        = binop (pchar '>') AtomParse AtomParse |>> fun x -> Conj (Not (AEq x), Not (ALt x))
    let GreaterOrEqualParse = binop (pstring ">=") AtomParse AtomParse |>> fun x -> Not (ALt x)
    let LessOrEqualParse    = binop (pstring "<=") AtomParse AtomParse |>> fun x -> Not (Conj ((Not (ALt x)), Not (Not (Not (AEq x)))))
    do bpref := choice [PrrParse; NotParse; LessOrEqualParse; TrueParse; FalseParse; EqualParse; NotEqualParse; LessParse; GreaterParse; GreaterOrEqualParse; ConParse; DisParse]

    let BexpParse = BTermParse

    let StmTermParse, stmref = createParserForwardedToRef<stm>()
    let StmProdParse, stmbref = createParserForwardedToRef<stm>()

    let SkipParse  = pstring "" |>> fun _ -> Skip
    let SeqParse   = binop (many whitespaceChar >>. pchar ';' .>> many whitespaceChar) StmProdParse StmTermParse |>> Seq
    let ITParse    = ((((((((((many whitespaceChar >>. pif) .>> spaces) >>. BProdParse) .>> spaces) .>> pthen) .>> spaces ) .>> pchar '{') .>> many whitespaceChar .>>. StmTermParse) .>> many whitespaceChar) .>> pchar '}') .>> many whitespaceChar .>>. SkipParse |>> fun ((x, y), z) -> ITE (x, y, z)
    let ITEParse   = ((((((((((((((many whitespaceChar >>. pif) .>> spaces) >>. BProdParse) .>> spaces) .>> pthen) .>> spaces) .>> pchar '{') .>> many whitespaceChar .>>. StmTermParse) .>> many whitespaceChar) .>> pchar '}') .>> many whitespaceChar) .>> pelse) .>> spaces) .>> pchar '{') .>> spaces .>>. StmProdParse .>> spaces .>> pchar '}' |>> fun ((x, y), z) -> ITE (x, y, z)
    let WhileParse = ((((((((many whitespaceChar >>. pwhile) .>> spaces) >>. BProdParse) .>> spaces) .>> pdo) .>> spaces) .>> pchar '{') .>> many whitespaceChar .>>. StmTermParse) .>> many whitespaceChar .>> pchar '}' |>> While

    do stmref := choice [SeqParse; WhileParse; ITEParse; ITParse; StmProdParse;]  

    let AssParse     = (((many whitespaceChar >>. pid .>> many whitespaceChar) .>> pstring ":=") .>> spaces) .>>. TermParse |>> fun (x, y) -> Ass (string x, y)
    let DeclareParse = many whitespaceChar .>> pdeclare .>> spaces1 >>. pid |>> Declare
    
    do stmbref := choice [DeclareParse; AssParse;] 

    let stmntParse = StmTermParse



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

