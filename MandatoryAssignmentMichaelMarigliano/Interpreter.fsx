#load "Parser.fs" 
open Parser

let rec lookup x = function
| []                      -> failwith ("unbound: " + x)
| (y, w) :: environment   -> if x = y then w else lookup x environment

let evalProg (funcs, exp) =
    let rec eval env = function
       | INT i                         -> i
       | NEG exp                       -> eval env exp * (-1)
       | ADD (exp1, exp2)              -> eval env exp1 + eval env exp2
       | SUB (exp1, exp2)              -> eval env exp1 - eval env exp2
       | MUL (exp1, exp2)              -> eval env exp1 * eval env exp2
       | DIV (exp1, exp2)              -> eval env exp1 / eval env exp2
       | EQ  (exp1, exp2)              -> if eval env exp1 = eval env exp2
                                            then eval env (INT(1)) 
                                            else eval env (INT(0))
       | NEQ (exp1, exp2)              -> if eval env exp1 <> eval env exp2
                                            then eval env (INT(1)) 
                                            else eval env (INT(0))
       | LT  (exp1, exp2)               -> if eval env exp1 < eval env exp2
                                            then eval env (INT(1)) 
                                            else eval env (INT(0))
       | LE  (exp1, exp2)               -> if eval env exp1 <= eval env exp2
                                            then eval env (INT(1)) 
                                            else eval env (INT(0))
       | GT  (exp1, exp2)               -> if eval env exp1 > eval env exp2
                                            then eval env (INT(1)) 
                                            else eval env (INT(0))
       | GE  (exp1, exp2)               -> if eval env exp1 >= eval env exp2
                                            then eval env (INT(1)) 
                                            else eval env (INT(0))
       | AND (exp1, exp2)               -> if eval env exp1 = 1 && eval env exp2 = 1
                                            then eval env (INT(1))
                                            else eval env (INT(0))
       | OR  (exp1, exp2)               -> if eval env exp1 = 1 || eval env exp2 = 1
                                            then eval env (INT(1))
                                            else eval env (INT(0))
       | VAR x                          -> lookup x env
       | LET (x, exp1, exp2)            -> let var = eval env exp1
                                           eval ((x, var) :: env) exp2
       | IF  (exp1, exp2, exp3)         -> if eval env exp1 = 1 
                                            then eval env exp2
                                            else eval env exp3
       | CALL (func, exp)               -> if exp.Length <> 0
                                             then
                                             let vals = List.map(fun x -> eval env x) exp
                                             let (vars, body) = lookup func funcs
                                             let varVals = List.zip vars vals
                                             eval varVals body
                                           else failwith "No Arguments"
                                                      

    eval [] exp

let Add = evalProg(parseProgFromString "1+1") 
let Sub = evalProg(parseProgFromString "1-2") 
let Mul = evalProg(parseProgFromString "2*2") 
let Div = evalProg(parseProgFromString "0/5") 
let Neg = evalProg(parseProgFromString "-1") 
let Eq = evalProg(parseProgFromString "1==1") 
let Neq = evalProg(parseProgFromString "1!=1") 
let Lt = evalProg(parseProgFromString "1<2") 
let Le = evalProg(parseProgFromString "2<=2") 
let Gt = evalProg(parseProgFromString "1>2") 
let Ge = evalProg(parseProgFromString "1>=2") 
let IfWithAnd = evalProg(parseProgFromString "if(1==1 && 1==1) then 1 else 0") 
let IfWithOr = evalProg(parseProgFromString "if(1==2 || 1==1) then 1 else 0")
let Let = evalProg(parseProgFromString "let x = 1 in 1 + x - 1") 
let Factorial = evalProg(parseProgFromString "func fac(n) =  if n > 0 then  n * fac(n - 1)else 1;fac(3)")
let MultipleParams = evalProg(parseProgFromString "func sum(x, y) = x + y; sum(1,1)") 


