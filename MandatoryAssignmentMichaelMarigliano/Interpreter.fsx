#load "Parser.fs"
#load "Helpers.fs" 
open Parser
open Helpers

//Implementation of the Interpreter
let evaluateProgram (functionsList, argumentExpression) =
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
        evaluate [] argumentExpression
        
        
let example = parseProgFromString "let c = 7 in if c == 6 then 4 else 3"
evaluateProgram example