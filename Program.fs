

open Microsoft.VisualBasic.CompilerServices



type VAL =
    | INT_ARRAY of VAL[]
    | INT of int
    | STR of string
    | RETURN of VAL
    | NONE

type AST =
    | IF of AST * AST * AST
    | VAR_ASSIGN of string * AST
    | ARR_ASSIGN of string * int * AST // name * index * value
    | ARR_DEF of AST // int array of length
    | INPUT_INT
    | OUTPUT of AST
    | CONST of VAL
    | PLUS_OP of AST * AST
    | VAR_REF of string
    | ARR_REF of string * int
    | RET_OP of AST
    | BLOCK of list<AST>   // BLOCK can return value and stop a program (or return NONE)
    
exception TypeNotExpected
exception VariableAlreadyDefined

exception VariableUndefined

let e =
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
    
    
let val_or_exception: Option<int> -> int = fun s -> s.Value


let print_val v = match v with
                  | INT x -> printfn "%i" x
                  | STR x -> printfn "%s" x
                  | _ -> printfn "?"


// i: (vars, ast) -> (vars, value)
let rec i (vars: Map<string, VAL>) expr = match expr with
                                            | BLOCK xs -> xs |> List.fold (fun (vars, _) ast -> i vars ast) (vars, NONE)
                                                             |> ignore; vars, NONE
                                            | OUTPUT x ->  (i vars x) |> snd |> print_val; vars, NONE
                                            | VAR_ASSIGN (name, v) -> if vars.ContainsKey name then raise VariableAlreadyDefined
                                                                      else vars.Add (name, v |> (i vars) |> snd), NONE 
                                            | IF (cond, yes, no) -> match snd (i vars cond) with
                                                                    | INT 0 -> i vars no
                                                                    | INT _ -> i vars yes
                                                                    | _ -> raise TypeNotExpected
                                            | INPUT_INT -> vars, INT (System.Console.ReadLine() |> IntegerType.FromString)
                                            | PLUS_OP (l,r) -> match snd (i vars l), snd (i vars r) with
                                                               | (INT a, INT b) -> vars, INT (a+b)
                                                               | _ -> raise TypeNotExpected
                                            | CONST x -> vars, x
                                            | VAR_REF name -> vars, if vars.ContainsKey name then vars.[name]
                                                                    else raise VariableUndefined
                                            | ARR_DEF x -> vars, match (i vars x) with
                                                                 | (_, INT x) ->  x |> Array.zeroCreate |> INT_ARRAY
                                                                 | _ -> raise TypeNotExpected
                                            | ARR_ASSIGN (name, index, x) -> vars, match (name |> VAR_REF |> (i vars) |> snd) with
                                                                                   | INT_ARRAY arr -> arr.[index] <- (match (x |> (i vars) |> snd) with
                                                                                                                      | INT x -> INT x
                                                                                                                      | _ -> raise TypeNotExpected); NONE
                                                                                   | _ -> raise TypeNotExpected
                                            | ARR_REF (name, index) -> vars, match (name |> VAR_REF |> (i vars) |> snd) with
                                                                                   | INT_ARRAY arr -> arr.[index]
                                                                                   | _ -> raise TypeNotExpected
                                            | RET_OP x -> vars, (RETURN (snd (i vars x)))

let result = i (Map.ofList []) e
