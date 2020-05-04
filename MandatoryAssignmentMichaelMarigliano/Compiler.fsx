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
                                            
                                            