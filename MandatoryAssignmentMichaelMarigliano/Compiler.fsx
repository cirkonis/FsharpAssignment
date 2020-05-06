#load "VirtualMachine.fs"
#load "Parser.fs"
#load "Helpers.fs"
open VM
open Parser
open Helpers

//Implementation of the Compiler
   let rec compile functionEnvironment environment = function
     | INT i                             -> [IPUSH i]
     | ADD (expression1, expression2)    -> compile functionEnvironment environment         expression1 @
                                            compile functionEnvironment ("" :: environment) expression2 @
                                            [IADD]
     | SUB (expression1, expression2)    -> compile functionEnvironment environment         expression1 @
                                            compile functionEnvironment ("" :: environment) expression2 @
                                            [ISUB]
     | MUL (expression1, expression2)    -> compile functionEnvironment environment         expression1 @
                                            compile functionEnvironment ("" :: environment) expression2 @
                                            [IMUL]
     | DIV (expression1, expression2)    -> compile functionEnvironment environment         expression1 @
                                            compile functionEnvironment ("" :: environment) expression2 @
                                            [IDIV]
     | NEG (expression)                  -> compile functionEnvironment environment (INT(0)) @
                                            compile functionEnvironment ("" :: environment) expression  @
                                            [ISUB]
     | VAR x                             -> [IGET (variablePosition x environment)]
     | LET (x, expression1, expression2) -> compile functionEnvironment environment         expression1 @
                                            compile functionEnvironment (x :: environment)  expression2 @
                                            [ISWAP]@
                                            [IPOP]
     | EQ (expression1, expression2)     -> compile functionEnvironment environment         expression1 @
                                            compile functionEnvironment ("" :: environment) expression2 @
                                            [IEQ]
     | NEQ (expression1, expression2)    -> compile functionEnvironment environment (INT(1)) @
                                            compile functionEnvironment ("" :: environment) (EQ(expression1, expression2)) @
                                            [ISUB]
     | LT (expression1, expression2)     -> compile functionEnvironment environment         expression1 @
                                            compile functionEnvironment ("" :: environment) expression2 @
                                            [ILT]
     | LE (expression1, expression2)     -> compile functionEnvironment environment         expression1 @
                                            compile functionEnvironment ("" :: environment) expression2 @
                                            [ILE]
     | GT (expression1, expression2)     -> compile functionEnvironment environment         expression2 @
                                            compile functionEnvironment ("" :: environment) expression1 @
                                            [ILT]
     | GE (expression1, expression2)     -> compile functionEnvironment environment         expression2 @
                                            compile functionEnvironment ("" :: environment) expression1 @
                                            [ILE]
     | IF (expression1, expression2, expression3)    -> let label2 = newLabel()
                                                        let label1 = newLabel()
                                                        compile functionEnvironment environment expression1 @
                                                        [IJMPIF label2]                 @
                                                        compile functionEnvironment environment expression3 @
                                                        [IJMP label1]                   @
                                                        [ILAB label2]                   @
                                                        compile functionEnvironment environment expression2 @
                                                        [ILAB label1]
     | AND (expression1, expression2)    -> compile functionEnvironment environment (INT(2))@
                                            compile functionEnvironment ("" :: environment) (expression1) @
                                            compile functionEnvironment ("" :: environment) (expression2) @
                                            [IADD]@
                                            [IEQ]
     | OR (expression1, expression2)    ->  compile functionEnvironment environment (INT(0)) @
                                            compile functionEnvironment ("" :: environment) (expression1) @
                                            compile functionEnvironment ("" :: environment) (expression2) @
                                            [IADD] @
                                            [ILT]
     | CALL (func, expression)          ->  let labReturn = newLabel()
                                            let labFunc = lookup func functionEnvironment
                                            let rec loopy expression = 
                                                      match expression with
                                                      | [] -> []
                                                      | e::exp -> 
                                                         compile functionEnvironment ("" :: environment) e @
                                                         loopy exp
                                            loopy expression
                                            [ICALL labFunc] @
                                            [ILAB  labReturn] @
                                            [ISWAP] @
                                            [IPOP]
 
 
 
                                            
//testing

    