#load "Parser.fs"
#load "Helpers.fs" 
open Parser
open Helpers

//Implementation of the Interpreter

let evaluateProgram (functionList, argumentExpression) =
    let rec evaluate environment = function
       | VAR x                                       -> lookup x environment
       | INT i                                       -> i
       | ADD (expression1, expression2)              -> evaluate environment expression1 + evaluate environment expression2
       | SUB (expression1, expression2)              -> evaluate environment expression1 - evaluate environment expression2
       | MUL (expression1, expression2)              -> evaluate environment expression1 * evaluate environment expression2
       | DIV (expression1, expression2)              -> evaluate environment expression1 / evaluate environment expression2
       | NEG expression                              -> evaluate environment expression * (-1)
       | EQ  (expression1, expression2)              -> if evaluate environment expression1 = evaluate environment expression2
                                                            then evaluate environment (INT(1)) 
                                                            else evaluate environment (INT(0))
       | NEQ (expression1, expression2)              -> if evaluate environment expression1 <> evaluate environment expression2
                                                            then evaluate environment (INT(1)) 
                                                            else evaluate environment (INT(0))
       | LT  (expression1, expression2)               -> if evaluate environment expression1 < evaluate environment expression2
                                                            then evaluate environment (INT(1)) 
                                                            else evaluate environment (INT(0))
       | LE  (expression1, expression2)               -> if evaluate environment expression1 <= evaluate environment expression2
                                                            then evaluate environment (INT(1)) 
                                                            else evaluate environment (INT(0))
       | GT  (expression1, expression2)               -> if evaluate environment expression1 > evaluate environment expression2
                                                            then evaluate environment (INT(1)) 
                                                            else evaluate environment (INT(0))
       | GE  (expression1, expression2)               -> if evaluate environment expression1 >= evaluate environment expression2
                                                            then evaluate environment (INT(1)) 
                                                            else evaluate environment (INT(0))
       | AND (expression1, expression2)               -> if evaluate environment expression1 = 1 && evaluate environment expression2 = 1
                                                            then evaluate environment (INT(1))
                                                            else evaluate environment (INT(0))
       | OR  (expression1, expression2)               -> if evaluate environment expression1 = 1 || evaluate environment expression2 = 1
                                                            then evaluate environment (INT(1))
                                                            else evaluate environment (INT(0))
       | LET (x, expression1, expression2)            -> let var = evaluate environment expression1
                                                         evaluate ((x, var) :: environment) expression2
       | IF  (expression1, expression2, expression3)  -> if evaluate environment expression1 = 1 
                                                         then evaluate environment expression2
                                                         else evaluate environment expression3
       //funkshun = function :) 
       | CALL (funkshun, expression)                  -> if expression.Length <> 0
                                                             then
                                                             let values = List.map(fun v -> evaluate environment v) expression
                                                             let (varnames, body) = lookup funkshun functionList
                                                             let zippy = List.zip varnames values
                                                             evaluate zippy body
                                                         else failwith "Function must have params"
                                                      

    evaluate [] argumentExpression
//Testing
let testAdd = evaluateProgram(parseProgFromString "2+2") //Should be 4
let testSub = evaluateProgram(parseProgFromString "2-2") //Should be 0
let testMul = evaluateProgram(parseProgFromString "2*3") //Should be 6
let testDiv = evaluateProgram(parseProgFromString "2/2") //Should be 1
let testNeg = evaluateProgram(parseProgFromString "-2") //Should be -2
let testEq = evaluateProgram(parseProgFromString "2==2") //Should be True
let testNeq = evaluateProgram(parseProgFromString "2!=2") //Should be False
let testLt = evaluateProgram(parseProgFromString "2<2") //Should be Talse
let testLe = evaluateProgram(parseProgFromString "2<=2") //Should be True
let testGt = evaluateProgram(parseProgFromString "3>2") //Should be True
let testGe = evaluateProgram(parseProgFromString "1>=2") //Should be False
let testIfAnd = evaluateProgram(parseProgFromString "if(2==2 && 3==3) then 1 else 0") //Should be True
let testIfOr = evaluateProgram(parseProgFromString "if(2==5 || 3==3) then 1 else 0") //Should be True 
let testSingleArgFunctionFactorial = evaluateProgram(parseProgFromString "func fac(n) =  if n > 0 then  n * fac(n - 1)else 1;fac(5)")
let testLet = evaluateProgram(parseProgFromString "let x = 15 in 4 + x * 7") //Should be ABiggerNumber than x
let testFfunctionMultipleArgs = evaluateProgram(parseProgFromString "func bigger(x, y) = if x > y then 1 else 0; bigger(2,4)") //Should be False(0)


