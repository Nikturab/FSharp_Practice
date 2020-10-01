

open Microsoft.VisualBasic.CompilerServices



type VAL =
    | INT of int
    | STR of string
    | RETURN of VAL
    | NONE

type AST =
    | IF of AST * AST * AST
    | VAR_ASSIGN of string * AST
    | INPUT_INT
    | OUTPUT of AST
    | CONST of VAL
    | PLUS_OP of AST * AST
    | VAR_REF of string
    | RET_OP of AST
    | BLOCK of list<AST>   // BLOCK can return value and stop a program (or return NONE)
    
exception TypeNotExpected

let e =
    BLOCK [ VAR_ASSIGN("$input", INPUT_INT);
                IF
                    (PLUS_OP(VAR_REF "$input", CONST (INT -4337)),
                     VAR_ASSIGN("$message", CONST (STR "Your input is not 4337")),
                     VAR_ASSIGN("$message", CONST (STR "Your input is 4337")));
                OUTPUT(VAR_REF "$message");
                RET_OP (CONST (INT 0)) ]
    
let arr = [1;2;3]

let val_or_exception: Option<int> -> int = fun s -> s.Value


let print_val v = match v with
                  | INT x -> printfn "%i" x
                  | STR x -> printfn "%s" x
                  | _ -> printfn "?"



let rec i (vars: Map<string, VAL>) expr = match expr with
                                            | BLOCK xs -> xs |> List.fold (fun (vars, _) ast -> i vars ast) (vars, NONE)
                                            | OUTPUT x ->  (i vars x) |> snd |> print_val; vars, NONE
                                            | VAR_ASSIGN (name, v) -> vars.Add (name, snd (i vars v)), NONE 
                                            | IF (cond, yes, no) -> match snd (i vars cond) with
                                                                    | INT 0 -> i vars no 
                                                                    | _ -> i vars yes 
                                            | INPUT_INT -> vars, INT (System.Console.ReadLine() |> IntegerType.FromString)
                                            | PLUS_OP (l,r) -> match snd (i vars l), snd (i vars r) with
                                                               | (INT a, INT b) -> vars, INT (a+b)
                                                               | _ -> raise (TypeNotExpected)
                                            | CONST x -> vars, x
                                            | VAR_REF name -> vars, vars.[name]
                                            | RET_OP x -> vars, (RETURN (snd (i vars x)))

let result = i (Map.ofList []) e
