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
                                        let loop =
                                            let rec loopy expression = 
                                                      match expression with
                                                      | [] -> []
                                                      | e::exp -> 
                                                         compile functionEnvironment ("" :: environment) e @
                                                         loopy exp
                                            loopy expression
                                        loop @    
                                        [ICALL labFunc] @
                                        [ILAB  labReturn] @
                                        [ISWAP] @
                                        [IPOP]
                                        

let compileProgram (listOfFunctions, expression) = 
        let functionEnvironment = List.map (fun (f, _) -> (f, newLabel())) listOfFunctions
        let rec compileFunctions = function
          | []                               -> compile functionEnvironment [] expression @
                                                [IHALT]
          | (f, (x, e)) :: listOfFunctions   -> let functionLabel = lookup f functionEnvironment
                                                compileFunctions listOfFunctions      @
                                                [ILAB functionLabel]@
                                                compile functionEnvironment ["";x] e  @
                                                [ISWAP]@
                                                [IRETN]
        compileFunctions listOfFunctions

                                        
//Testing
//Individual Component Testing
let compileSimpleAdd = compile [] [] (ADD(INT 1, INT 2)) 
let CompiledSimpleAddAnswer = execProg compileSimpleAdd [] //Should be 3
let compileSimpleSub = compile [] [] (SUB(INT 1, INT 2))
let CompiledSimpleSubAnswer = execProg compileSimpleSub [] //Should be -1
let compileSimpleMul = compile [] [] (MUL(INT 5, INT 5))
let CompiledSimpleMulAnswer = execProg compileSimpleMul [] //Should be 25
let compileSimpleDiv = compile [] [] (DIV(INT 0, INT 5)) 
let CompiledSimpleDivAnswer = execProg compileSimpleDiv [] //Should be 0. Testing Div by zero results in error
let compileSimpleNeg = compile [] [] (NEG(INT 4))
let CompiledSimpleNegAnswer = execProg compileSimpleNeg [] //Should be -4
let compileSimpleEq = compile [] [] (EQ(INT 4, INT 3)) 
let CompiledSimpleEqAnswer = execProg compileSimpleEq [] //Should be False(0)
let compileSimpleNeq = compile [] [] (NEQ(INT 4, INT 3)) 
let CompiledSimpleNeqAnswer = execProg compileSimpleNeq [] //Should be True(1)

let compileSimpleLet = compile [] [] (LET("x", INT(5), ADD(VAR "x", INT 5)))
let CompiledSimpleLetAnswer = execProg compileSimpleLet [] //Should be 10
let compileSimpleGe = compile [] [] (GE(INT(6), INT(5)))
let CompiledSimpleGeAnswer = execProg compileSimpleGe [] //Should be True(1)
let compileSimpleGt = compile [] [] (GT(INT(4), INT(5)))
let CompiledSimpleGtAnswer = execProg compileSimpleGt [] //Should be False(1)
let compileSimpleLe = compile [] [] (LE(INT(5), INT(5)))
let CompiledSimpleLeAnswer = execProg compileSimpleLe [] //Should be True(1)
let compileSimpleLt = compile [] [] (LT(INT(4), INT(5)))
let CompiledSimpleLtAnswer = execProg compileSimpleLt [] //Should be True(1)

let compileSimpleIfAnd = compile [] [] (IF(AND(EQ(INT(5), INT(5)),EQ(INT(4),INT(4))),INT(1),INT(0)))
let CompiledSimpleIfAndAnswer = execProg compileSimpleIfAnd [] //Should be True(1)
let compileSimpleIfOr = compile [] [] (IF(OR(EQ(INT(5), INT(5)),EQ(INT(3),INT(4))),INT(1),INT(0)))
let CompiledSimpleIfOrAnswer = execProg compileSimpleIfOr [] //Should be True(1)

//FunctionTesting
let compileFunctionSingleArgument = compileProgram([("Add2", ("x", ADD(INT 2, VAR "x")))], (CALL("Add2", [INT 5])))
let compileFunctionSingleArgumentAnswer = execProg compileFunctionSingleArgument []
//FactorialTest error with indexing
//let compileFactorial = compileProgram ([("fac", ("n",IF (GT (VAR "n",INT 0),MUL (VAR "n",CALL ("fac",[SUB (VAR "n",INT 1)])), INT 1)))], (CALL ("fac",[INT 5])))
//let compileFactorialAnswer = execProg compileFactorial []
//Multiple Argurment Function Not Implemented
//let compileFunctionMultipleArguments = compileProgram([("f", ("x, y", ADD(VAR "y", VAR "x")))], (CALL("f", [INT 5, INT 4])))
//let compileFunctionMultipleArgumentsAnswer = execProg compileFunctionMultipleArguments
    
