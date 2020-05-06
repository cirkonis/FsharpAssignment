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
                                                        let labele = newLabel()
                                                        compile functionEnvironment environment expression1 @
                                                        [IJMPIF label2]                 @
                                                        compile functionEnvironment environment expression3 @
                                                        [IJMP labele]                   @
                                                        [ILAB label2]                   @
                                                        compile functionEnvironment environment expression2 @
                                                        [ILAB labele]
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
     
     
     
(*     |CALL (funkshun, expression)        -> let returnLabel = newLabel()
                                            let functionLabel = lookup funkshun functionEnvironment
                                            compile functionEnvironment environment expression @
                                            [ICALL functionLabel] @
                                            [ILAB returnLabel]    @
                                            [ISWAP]               @
                                            [IPOP];;

let compileProgram (listOfFunctions, expression) = 
        let functionEnvironment = List.map (fun (f, _) -> (f, newLabel())) listOfFunctions
        let rec compileFunctions = function
          | []                               -> compile functionEnvironment [] expression @
                                                [IHALT]
          | (f, (x, e)) :: listOfFunctions   -> let functionLabel = lookup f functionEnvironment
                                                compileFunctions listOfFunctions      @
                                                [ILAB functionLabel]                  @
                                                compile functionEnvironment [""; x] e @
                                                [ISWAP]                               @
                                                [IRETN]
        compileFunctions listOfFunctions;;
 *)
 (*
 ////TESTING AND and OR
 /// 

    let andTest = compile [] [] (IF(AND(EQ(INT(7),INT(7)),EQ(INT(5),INT(4))),INT(1),INT(0)))
    let andTestAnswer = execProg andTest [] 
   
    let orTest = compile [] [] (IF(OR(EQ(INT(7),INT(6)),EQ(INT(5),INT(4))),INT(1),INT(0)))
    let orTestAnswer = execProg orTest [] 
   *)
   (*     
        ///////////////TESTING LT AND LE
        
    let ltTest = compile [] [] (LT(INT(2),INT(1)))
    let leTest = compile [] [] (LE(INT(2),INT(1)))
    let gtTest = compile [] [] (GT(INT(2),INT(1)))
    let geTest = compile [] [] (GE(INT(2),INT(1)))

    let ltTestAnswer = execProg ltTest []
    let leTestAnswer = execProg leTest []    
    let gtTestAnswer = execProg gtTest []    
    let geTestAnswer = execProg geTest []    
        
        
        

//use this to get most of the stuff to compile from the parser
   let addy = parseExpFromString "let x = 7 in 5+7+x"
   //examples of using the compiler with out functions 
   let compily = compile [] [] (ADD(INT(5), INT(2)))
   let compily2 = compile [] [] (LET(("x"),INT(7),ADD(ADD(INT(5),INT(7)),VAR("x"))))
   let answerToCompily = execProg compily2 []
   
//////////////////////TESTING
   let eqTest = parseExpFromString "1==1" //just to get the bearing, not needed
   let eqCompileTest = compile [] [] (EQ(INT(1),INT(2)))
   let eqTestAnswer = execProg eqCompileTest []
   
   let neqTest = compile [] [] (NEQ(INT(1),INT(1)))
   let neqTestAnswer = execProg neqTest []
   *)