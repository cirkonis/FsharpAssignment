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
        evaluate [] argumentExpression
        
        
let example = parseProgFromString "4>=4"
evaluateProgram example