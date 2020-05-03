#load "Parser.fs"
#load "Helpers.fs"
open Parser
open Helpers


let evaluateProgram (functionsList, argumentExpression) =
        let rec evaluate environment = function
           | VAR x                                       -> lookup x environment
           | INT i                                       -> i
           | ADD (expression1, expression2)              -> evaluate environment expression1 + evaluate environment expression2
           | SUB (expression1, expression2)              -> evaluate environment expression1 - evaluate environment expression2
           | MUL (expression1, expression2)              -> evaluate environment expression1 * evaluate environment expression2
           | DIV (expression1, expression2)              -> evaluate environment expression1 / evaluate environment expression2
        evaluate [] argumentExpression
        
        
let example = parseProgFromString "4/4"
evaluateProgram example