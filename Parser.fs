
module Interpreter.Parser
open FParsec

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
    
let str s = pstring s
let ws = spaces
let str_ws s = pstring s .>> ws
let identifier =
    let isIdentifierFirstChar c = c = '$'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
    .>> ws // skips trailing whitespace
            
let pval = identifier
let stringLiteral: Parser<string,unit> =
    between (pstring "\"") (pstring "\"")
            (manyChars (satisfy (fun c -> c <> '"')))
let inumber: Parser<AST,unit> = pint32 |>> INT |>> CONST
let istring = stringLiteral |>> STR |>> CONST
let jvalue, jvalueRef = createParserForwardedToRef<AST, unit>()

let iassignment: Parser<AST,unit> = pipe4 (str_ws "var") identifier (str_ws "=") jvalue (fun _ x _ z -> VAR_ASSIGN(x, z))
let SepBySemiColon pElement f =
    (ws >>. (sepEndBy (pElement .>> ws) (str ";" >>. ws))  |>> f)
let IfCond pElement =
    between (str_ws "if(") (str_ws "):") (ws >>. pElement)
            
let iblock   = SepBySemiColon jvalue BLOCK
let iinputInt = (str_ws "input_int") |>> (fun _ -> INPUT_INT)
let ioutput = pipe2 (str_ws "output") jvalue (fun _ x -> OUTPUT x)
let iif = pipe5 (IfCond jvalue) iblock (str_ws "else:") iblock (str_ws "endif") (fun cond yes _ no _ -> IF (cond, yes, no))

let iidentifier = identifier |>> VAR_REF

do jvalueRef := choice [
   iidentifier; inumber; istring; iassignment; iinputInt; ioutput; iif
]
test iblock ""


let test_program = // test program with arrays
    BLOCK [VAR_ASSIGN("$input", INPUT_INT);
                IF
                    (PLUS_OP(VAR_REF "$input", CONST (INT -4337)),
                     "Your input is not 4337" |> STR |> CONST |> OUTPUT,
                     BLOCK [
                        "Your input is 4337. Enter another number: " |> STR |> CONST |> OUTPUT;
                        VAR_ASSIGN("$array", 3 |> INT |> CONST |> ARR_DEF);
                        ARR_ASSIGN("$array", 0, INPUT_INT);
                        "Your number: " |> STR |> CONST |> OUTPUT
                        OUTPUT (ARR_REF ("$array", 0)); ]);
                 RET_OP (CONST (INT 0)) ]
