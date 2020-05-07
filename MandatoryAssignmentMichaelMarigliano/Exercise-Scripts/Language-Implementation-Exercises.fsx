(*
(*
//SECTION 1

//1.1 Representing arithmetic expressions

type expression = | INT of int
                  | ADD of expression * expression;;

//1.2 Interpreting arithmetic expressions

let rec evaluate = function
    | INT i         -> i
    | ADD (expression1, expression2)  -> evaluate expression1 + evaluate expression2;;

//example evaluate(ADD(INT(x), INT(y)));; int = x + y


//1.3 Instruction set for arithmetic expressions (low level target language)

type instruction = |IPUSH of int
                   |IADD;;

//1.4 "Virtual Machine" for arithmetic expressions
let rec execute instructions stack = 
    match (instructions, stack) with
      |([],                      v::_)              -> v
      |(IPUSH i :: instructions, stack)             -> execute instructions (i    :: stack)
      |(IADD    :: instructions, y :: x :: stack)   -> execute instructions (x + y :: stack);;

//1.5 Compiling Arithmetic expressions
let rec compile = function
  | INT i                          -> [IPUSH i]
  | ADD (expression1, expression2) -> compile expression1 @
                                      compile expression2 @
                                      [IADD];;



 *)


//1.6 EXERCISES
//exercise 1.1  - extend the interpreter, VM, and compiler to be able to handle multiplying integers
(*
type expression = | INT of int
                  | ADD of expression * expression
                  | MUL of expression * expression;;



let rec evaluate = function
    | INT i         -> i
    | ADD (expression1, expression2)  -> evaluate expression1 + evaluate expression2
    | MUL (expression1, expression2)  -> evaluate expression1 * evaluate expression2;;


type instruction = |IPUSH of int
                   |IADD
                   |IMUL;;



let rec execute instructions stack = 
    match (instructions, stack) with
      |([],                      v::_)              -> v
      |(IPUSH i :: instructions, stack)             -> execute instructions (i    :: stack)
      |(IADD    :: instructions, y :: x :: stack)   -> execute instructions (x + y :: stack)
      |(IMUL    :: instructions, y :: x :: stack)   -> execute instructions (x * y :: stack);;

let rec compile = function
  | INT i                          -> [IPUSH i]
  | ADD (expression1, expression2) -> compile expression1 @
                                      compile expression2 @
                                      [IADD]
  | MUL (expression1, expression2) -> compile expression1 @
                                      compile expression2 @
                                      [IMUL];;

 *)

//exercise 1.2 - extend further to include subtraction
(*
type expression = | INT of int
                  | ADD of expression * expression              
                  | SUB of expression * expression
                  | MUL of expression * expression;;



let rec evaluate = function
    | INT i         -> i
    | ADD (expression1, expression2)  -> evaluate expression1 + evaluate expression2
    | SUB (expression1, expression2)  -> evaluate expression1 - evaluate expression2
    | MUL (expression1, expression2)  -> evaluate expression1 * evaluate expression2;;


type instruction = |IPUSH of int
                   |IADD
                   |ISUB
                   |IMUL;;



let rec execute instructions stack = 
    match (instructions, stack) with
      |([],                      v::_)              -> v
      |(IPUSH i :: instructions, stack)             -> execute instructions (i    :: stack)
      |(IADD    :: instructions, y :: x :: stack)   -> execute instructions (x + y :: stack)
      |(ISUB    :: instructions, y :: x :: stack)   -> execute instructions (x - y :: stack)
      |(IMUL    :: instructions, y :: x :: stack)   -> execute instructions (x * y :: stack);;


let rec compile = function
  | INT i                          -> [IPUSH i]
  | ADD (expression1, expression2) -> compile expression1 @
                                      compile expression2 @
                                      [IADD]
  | SUB (expression1, expression2) -> compile expression1 @
                                      compile expression2 @
                                      [ISUB]
  | MUL (expression1, expression2) -> compile expression1 @
                                      compile expression2 @
                                      [IMUL];;


   *)

//exercise 1.3 - include a negation operation 
 (*
type expression = | INT of int
                  | ADD of expression * expression              
                  | SUB of expression * expression
                  | MUL of expression * expression
                  | NEG of expression;;



let rec evaluate = function
    | INT i         -> i
    | ADD (expression1, expression2)  -> evaluate expression1 + evaluate expression2
    | SUB (expression1, expression2)  -> evaluate expression1 - evaluate expression2
    | MUL (expression1, expression2)  -> evaluate expression1 * evaluate expression2
    | NEG expression                  -> evaluate expression * (-1);;


type instruction = |IPUSH of int
                   |IADD
                   |ISUB
                   |IMUL;;



let rec execute instructions stack = 
    match (instructions, stack) with
      |([],                      v::_)              -> v
      |(IPUSH i :: instructions, stack)             -> execute instructions (i    :: stack)
      |(IADD    :: instructions, y :: x :: stack)   -> execute instructions (x + y :: stack)
      |(ISUB    :: instructions, y :: x :: stack)   -> execute instructions (x - y :: stack)
      |(IMUL    :: instructions, y :: x :: stack)   -> execute instructions (x * y :: stack);;


let rec compile = function
  | INT i                          -> [IPUSH i]
  | ADD (expression1, expression2) -> compile expression1 @
                                      compile expression2 @
                                      [IADD]
  | SUB (expression1, expression2) -> compile expression1 @
                                      compile expression2 @
                                      [ISUB]
  | MUL (expression1, expression2) -> compile expression1 @
                                      compile expression2 @
                                      [IMUL]
  | NEG (expression)               -> compile(INT(0)) @
                                      compile expression @
                                      [ISUB];;


 *)


 

 //SECTION 2


 //2.1 Representing global changes

 type variable = string;;


 type expression = | VAR of variable 
                   | INT of int
                   | ADD of expression * expression              
                   | SUB of expression * expression
                   | MUL of expression * expression
                   | NEG of expression;;


type 'a environment = (variable * 'a) list;;

let rec lookup x = function
    | []                      -> failwith ("unbound: " + x)
    | (y, w) :: environment   -> if x = y then w else lookup x environment;;


//2.2 interpreting global variables

 let rec evaluate environment = function
     | VAR x                           -> lookup x environment
     | INT i                           -> i
     | ADD (expression1, expression2)  -> evaluate environment expression1 + evaluate environment expression2
     | SUB (expression1, expression2)  -> evaluate environment expression1 - evaluate environment expression2
     | MUL (expression1, expression2)  -> evaluate environment expression1 * evaluate environment expression2
     | NEG expression                  -> evaluate environment expression * (-1);;


 //2.3 instruction for gloabial variables

 type instruction = |IGET of int
                    |IPUSH of int
                    |IADD
                    |ISUB
                    |IMUL;;


 //2.4 virtual machine for global variables
 let get = List.item;;

 let rec execute instructions stack = 
     match (instructions, stack) with
       |([],                      v::_)              -> v
       |(IPUSH i :: instructions, stack)             -> execute instructions (i    :: stack)
       |(IADD    :: instructions, y :: x :: stack)   -> execute instructions (x + y :: stack)
       |(ISUB    :: instructions, y :: x :: stack)   -> execute instructions (x - y :: stack)
       |(IMUL    :: instructions, y :: x :: stack)   -> execute instructions (x * y :: stack)
       |(IGET  p :: instructions, stack)             -> execute instructions (get p stack :: stack);;




 //2.5 compiling for global variables

 let rec variablePosition x = function
        | []                -> failwith ("unbound: " + x)
        | y :: environment  -> if x = y then 0 else 1 + variablePosition x environment;;


 let rec compile environment = function
   | INT i                          -> [IPUSH i]
   | ADD (expression1, expression2) -> compile environment expression1 @
                                       compile ("" :: environment) expression2 @
                                       [IADD]
   | SUB (expression1, expression2) -> compile environment expression1 @
                                       compile ("" :: environment) expression2 @
                                       [ISUB]
   | MUL (expression1, expression2) -> compile environment expression1 @
                                       compile ("" :: environment) expression2 @
                                       [IMUL]
   | NEG (expression)               -> compile environment (INT(0)) @
                                       compile ("" :: environment) expression @
                                       [ISUB]
   | VAR x                          -> [IGET (variablePosition x environment)];;
   

   //2.6 EXERCISES

   //exercise 2.1 Run the interpreter with the example environment a + 5

   let interpretationEnvironemnt = [("pi", 3); ("bigNumber", 100000); ("a", 42)];;

   evaluate interpretationEnvironemnt (ADD(INT(5), VAR("a")));;

   //exercise 2.2 compile the expression a + 5 wiht the compile time environment then execute using the initial runtime stack

   let compileTimeEnvironment = ["pi";"bignumber";"a"];;

   let compile2_2 = compile compileTimeEnvironment (ADD(INT(5), VAR("a")));;

   let runTimeStack = [3; 10000; 42];;

   execute compile2_2 runTimeStack;; 

   //exercise 2.3 compile and execute teh expression (pi + a) + (pi + a) using the compile time environment above, then execute with the VM using the same runtime stack

   let compile2_3 = compile compileTimeEnvironment (ADD(ADD(VAR("pi"), VAR("a")),ADD(VAR("pi"),VAR("a"))));;

   execute compile2_3 runTimeStack;;
   


   //SECTION 3
   (*
   //3.1 Representing Local Variables

   type variable = string;;

   type expression = | LET of variable * expression * expression
                     | VAR of variable 
                     | INT of int
                     | ADD of expression * expression              
                     | SUB of expression * expression
                     | MUL of expression * expression
                     | NEG of expression;;


  type 'a environment = (variable * 'a) list;;

  let rec lookup x = function
      | []                      -> failwith ("unbound: " + x)
      | (y, w) :: environment   -> if x = y then w else lookup x environment;;


  //3.2 Interpreting local variables

   let rec evaluate environment = function
       | VAR x                             -> lookup x environment
       | INT i                             -> i
       | ADD (expression1, expression2)    -> evaluate environment expression1 + evaluate environment expression2
       | SUB (expression1, expression2)    -> evaluate environment expression1 - evaluate environment expression2
       | MUL (expression1, expression2)    -> evaluate environment expression1 * evaluate environment expression2
       | NEG expression                    -> evaluate environment expression * (-1)
       | LET (x, expression1, expression2) -> let var = evaluate environment expression1
                                              evaluate ((x, var) :: environment) expression2;;


   //3.3 instruction for local variables

   type instruction = |IGET of int
                      |IPUSH of int
                      |IADD
                      |ISUB
                      |IMUL
                      |IPOP
                      |ISWAP;;


   //3.4 virtual machine for local variables
   let get = List.item;;

   let rec execute instructions stack = 
       match (instructions, stack) with
         |([],                      v::_)              -> v
         |(IPUSH i :: instructions, stack)             -> execute instructions (i    :: stack)
         |(IADD    :: instructions, y :: x :: stack)   -> execute instructions (x + y :: stack)
         |(ISUB    :: instructions, y :: x :: stack)   -> execute instructions (x - y :: stack)
         |(IMUL    :: instructions, y :: x :: stack)   -> execute instructions (x * y :: stack)
         |(IGET  p :: instructions,           stack)   -> execute instructions (get p stack :: stack)
         |(IPOP    :: instructions,        _::stack)   -> execute instructions stack
         |(ISWAP   :: instructions, y :: x :: stack)   -> execute instructions (x :: y :: stack);;




   //3.5 compiling for local variables

   let rec variablePosition x = function
          | []                -> failwith ("unbound: " + x)
          | y :: environment  -> if x = y then 0 else 1 + variablePosition x environment;;


   let rec compile environment = function
     | INT i                             -> [IPUSH i]
     | ADD (expression1, expression2)    -> compile environment         expression1 @
                                            compile ("" :: environment) expression2 @
                                            [IADD]
     | SUB (expression1, expression2)    -> compile environment         expression1 @
                                            compile ("" :: environment) expression2 @
                                            [ISUB]
     | MUL (expression1, expression2)    -> compile environment         expression1 @
                                            compile ("" :: environment) expression2 @
                                            [IMUL]
     | NEG (expression)                  -> compile environment (INT(0)) @
                                            compile ("" :: environment) expression @
                                            [ISUB]
     | VAR x                             -> [IGET (variablePosition x environment)]
     | LET (x, expression1, expression2) -> compile environment        expression1 @
                                            compile (x :: environment) expression2 @
                                            [ISWAP]                                @
                                            [IPOP];;

//Section 3 EXERCISES

//exercise 3.1 extend the language with mutiplication and subtraction -- pretty sure I did this by not starting over in section 2

//exercise 3.2 run the progam (let x = 5 * 8 in x + x) + 7 

evaluate [] (ADD(LET(("x"), MUL(INT(5), INT(8)), ADD(VAR("x"), VAR("x"))), INT(7)));;

//exercise 3.3 

let compile3_3 = compile [] (ADD(LET(("x"), MUL(INT(5), INT(8)), ADD(VAR("x"), VAR("x"))), INT(7)));;

execute compile3_3 [];;

//exercise 3.4 compile the programs 1 + (2 + (3 + 4)) & ((4+ 3) + 2) + 1

let compile3_4_1 = compile [] (ADD(INT(1), ADD(INT(2), ADD(INT(3),INT(4)))));;

let compile3_4_2 = compile [] (ADD (ADD (ADD(INT(4), INT(3)), INT(2)), INT(1)));;

execute compile3_4_1 [];;

execute compile3_4_2 [];;

//^^^^verify and count cell stack count by hand 
//3_4_1 cell stack count - 
//3_4_2 cell stack count -   I would argue they are the same, dont know exactly what the cell stack count is but the instructions have the 
// same components, 4 push and three add
*)

//SECTION 4 Conditional Expressions
(*

//4.1 representing conditional expressions
   type variable = string;;

   type expression = | EQ  of expression * expression
                     | IF  of expression * expression * expression 
                     | LET of variable * expression * expression
                     | VAR of variable 
                     | INT of int
                     | ADD of expression * expression              
                     | SUB of expression * expression
                     | MUL of expression * expression
                     | NEG of expression;;


  type 'a environment = (variable * 'a) list;;

  let rec lookup x = function
      | []                      -> failwith ("unbound: " + x)
      | (y, w) :: environment   -> if x = y then w else lookup x environment;;


  //4.2 Interpretting Conditional Expressions

   let rec evaluate environment = function
       | VAR x                                       -> lookup x environment
       | INT i                                       -> i
       | ADD (expression1, expression2)              -> evaluate environment expression1 + evaluate environment expression2
       | SUB (expression1, expression2)              -> evaluate environment expression1 - evaluate environment expression2
       | MUL (expression1, expression2)              -> evaluate environment expression1 * evaluate environment expression2
       | NEG expression                              -> evaluate environment expression * (-1)
       | LET (x, expression1, expression2)           -> let var = evaluate environment expression1
                                                        evaluate ((x, var) :: environment) expression2
       | EQ  (expression1, expression2)              -> if evaluate environment expression1 = evaluate environment expression2 
                                                        then evaluate environment (INT(1)) 
                                                        else evaluate environment (INT(0))
       | IF  (expression1, expression2, expression3) -> if evaluate environment expression1 = 1 
                                                        then evaluate environment expression2
                                                        else evaluate environment expression3;;
                                            


   //4.3 instruction for Conditional Expressions
   type label = int;;
   type instruction = |IGET    of int
                      |IPUSH   of int
                      |IADD
                      |ISUB
                      |IMUL
                      |IPOP
                      |ISWAP
                      |IEQ
                      |ILAB    of label
                      |IJMP    of label
                      |IJMPIF  of label
                      |IHALT;;


   //4.4 virtual machine for Condtional Expressions
   let get = List.item;;

   let rec find listOfInstructions = function
    |[]                                           -> failwith "error executing code - generated by listOfInstructions function"
    |(ILAB listOfInstructions' :: instructions)   -> if listOfInstructions = listOfInstructions' 
                                                        then instructions
                                                        else find listOfInstructions instructions
    |(_                        :: instructions)   -> find listOfInstructions instructions;;


 let executeProgram program stack =
        let rec execute instructions stack = 
             match (instructions, stack) with
                 |([],                      v::_)              -> v
                 |(IHALT   :: _,            v::_)              -> v
                 |(IPUSH i :: instructions,           stack)   -> execute instructions (i    :: stack)
                 |(IGET  p :: instructions,           stack)   -> execute instructions (get p stack :: stack)
                 |(IADD    :: instructions, y :: x :: stack)   -> execute instructions (x + y :: stack)
                 |(ISUB    :: instructions, y :: x :: stack)   -> execute instructions (x - y :: stack)
                 |(IMUL    :: instructions, y :: x :: stack)   -> execute instructions (x * y :: stack)
                 |(IEQ     :: instructions, y :: x :: stack)   -> if x = y 
                                                                    then execute instructions (1 :: stack) 
                                                                    else execute instructions (0 :: stack)
                 |(IJMP l  :: _,                 _ :: stack)   -> execute (find l program) stack
                 |(IJMPIF l:: _,                 1 :: stack)   -> execute (find l program) stack
                 |(IJMPIF l:: instructions,      _ :: stack)   -> execute instructions stack
                 |(ILAB l  :: instructions,           stack)   -> execute instructions stack

                 |(IPOP    :: instructions,        _::stack)   -> execute instructions stack
                 |(ISWAP   :: instructions, y :: x :: stack)   -> execute instructions (x :: y :: stack)
        execute program stack;;
         




   //4.5 compiling for Condtional Expressions
   let mutable labelCounter = 0;;

   let newLabel _ = 
       let this = labelCounter
       labelCounter <- this + 1;
       this;;

   let rec variablePosition x = function
          | []                -> failwith ("unbound: " + x)
          | y :: environment  -> if x = y then 0 else 1 + variablePosition x environment;;


   let rec compile environment = function
     | INT i                             -> [IPUSH i]
     | ADD (expression1, expression2)    -> compile environment         expression1 @
                                            compile ("" :: environment) expression2 @
                                            [IADD]
     | SUB (expression1, expression2)    -> compile environment         expression1 @
                                            compile ("" :: environment) expression2 @
                                            [ISUB]
     | MUL (expression1, expression2)    -> compile environment         expression1 @
                                            compile ("" :: environment) expression2 @
                                            [IMUL]
     | NEG (expression)                  -> compile environment (INT(0)) @
                                            compile ("" :: environment) expression  @
                                            [ISUB]
     | VAR x                             -> [IGET (variablePosition x environment)]
     | LET (x, expression1, expression2) -> compile environment         expression1 @
                                            compile (x :: environment)  expression2 @
                                            [ISWAP]                                 @
                                            [IPOP]
     | EQ (expression1, expression2)     -> compile environment         expression1 @
                                            compile ("" :: environment) expression2 @
                                            [IEQ]
     | IF (expression1, expression2, expression3)    -> let label2 = newLabel()
                                                        let labele = newLabel()
                                                        compile environment expression1 @
                                                        [IJMPIF label2]                 @
                                                        compile environment expression3 @
                                                        [IJMP labele]                   @
                                                        [ILAB label2]                   @
                                                        compile environment expression2 @
                                                        [ILAB labele];;

//Sectio 4.6 EXERCISES
//merge the languages thus far, pretty sure I did that
//add in the interpreter equal and if lines-- did it before hand, interavtive window doesnt complain but no idea how to test as of now 
*)

(*
//SECTION 5 Global First Order Functions..... first order - NOT higher order functions 

//5.1 representing first order functions

   type variable = string;;

   type functionName = string;; 

   type expression = | EQ  of  expression * expression
                     | IF  of  expression * expression * expression 
                     | LET of  variable * expression * expression
                     | VAR of  variable 
                     | INT of  int
                     | ADD of  expression * expression              
                     | SUB of  expression * expression
                     | MUL of  expression * expression
                     | NEG of  expression
                     | CALL of functionName * expression;;

  type  funkshun = functionName * (variable * expression);;     //function = funkshun :D 

  type 'a environment = (variable * 'a) list;;

  let rec lookup x = function
      | []                      -> failwith ("unbound: " + x)
      | (y, w) :: environment   -> if x = y then w else lookup x environment;;


  //5.2 Interpretting First Order Functions
let evaluateProgram (listOfFunctions, argumentExpression) =
        let rec evaluate environment = function
           | VAR x                                       -> lookup x environment
           | INT i                                       -> i
           | ADD (expression1, expression2)              -> evaluate environment expression1 + evaluate environment expression2
           | SUB (expression1, expression2)              -> evaluate environment expression1 - evaluate environment expression2
           | MUL (expression1, expression2)              -> evaluate environment expression1 * evaluate environment expression2
           | NEG expression                              -> evaluate environment expression * (-1)
           | LET (x, expression1, expression2)           -> let var = evaluate environment expression1
                                                            evaluate ((x, var) :: environment) expression2
           | EQ  (expression1, expression2)              -> if evaluate environment expression1 = evaluate environment expression2 
                                                            then evaluate environment (INT(1)) 
                                                            else evaluate environment (INT(0))
           | IF  (expression1, expression2, expression3) -> if evaluate environment expression1 = 1 
                                                            then evaluate environment expression2
                                                            else evaluate environment expression3
           | CALL (funk, argumentExpression)             -> let value = evaluate environment argumentExpression
                                                            let (name, body) = lookup funk listOfFunctions
                                                            evaluate [(name, value)] body
        evaluate [] argumentExpression;;

   //5.3 instruction for first order functions
   type label = int;;
   type instruction = |IGET    of int
                      |IPUSH   of int
                      |IADD
                      |ISUB
                      |IMUL
                      |IPOP
                      |ISWAP
                      |IEQ
                      |ILAB    of label
                      |IJMP    of label
                      |IJMPIF  of label
                      |ICALL   of label
                      |IRETN
                      |IHALT;;


   //5.4 virtual machine for first order functions
   let get = List.item;;

   let rec find listOfInstructions = function
    |[]                                           -> failwith "error executing code - generated by listOfInstructions function"
    |(ILAB listOfInstructions' :: instructions)   -> if listOfInstructions = listOfInstructions' 
                                                        then instructions
                                                        else find listOfInstructions instructions
    |(_                        :: instructions)   -> find listOfInstructions instructions;;


 let executeProgram program stack =
        let rec execute instructions stack = 
             match (instructions, stack) with
                 |([],                      v::_)              -> v
                 |(IHALT   :: _,            v::_)              -> v
                 |(IPUSH i :: instructions,           stack)   -> execute instructions (i    :: stack)
                 |(IGET  p :: instructions,           stack)   -> execute instructions (get p stack :: stack)
                 |(IADD    :: instructions, y :: x :: stack)   -> execute instructions (x + y :: stack)
                 |(ISUB    :: instructions, y :: x :: stack)   -> execute instructions (x - y :: stack)
                 |(IMUL    :: instructions, y :: x :: stack)   -> execute instructions (x * y :: stack)
                 |(IEQ     :: instructions, y :: x :: stack)   -> if x = y 
                                                                    then execute instructions (1 :: stack) 
                                                                    else execute instructions (0 :: stack)
                 |(IJMP l  :: _,                 _ :: stack)   -> execute (find l program) stack
                 |(IJMPIF l:: _,                 1 :: stack)   -> execute (find l program) stack
                 |(IJMPIF l:: instructions,      _ :: stack)   -> execute instructions stack
                 |(ILAB l  :: instructions,           stack)   -> execute instructions stack

                 |(IPOP    :: instructions,        _::stack)   -> execute instructions stack
                 |(ISWAP   :: instructions, y :: x :: stack)   -> execute instructions (x :: y :: stack)
                 |(ICALL p :: ILAB l          :: _,   stack)   -> execute (find p program)  (l :: stack)
                 |(IRETN   :: _,                 l :: stack)   -> execute (find l program) stack
        execute program stack;;
         




   //5.5 compiling for first order funcitons
  
   let mutable labelCounter = 0;;

   let newLabel _ = 
       let this = labelCounter
       labelCounter <- this + 1;
       this;;

   let rec variablePosition x = function
          | []                -> failwith ("unbound: " + x)
          | y :: environment  -> if x = y then 0 else 1 + variablePosition x environment;;


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
     | NEG (expression)                  -> compile functionEnvironment environment (INT(0)) @
                                            compile functionEnvironment ("" :: environment) expression  @
                                            [ISUB]
     | VAR x                             -> [IGET (variablePosition x environment)]
     | LET (x, expression1, expression2) -> compile functionEnvironment environment         expression1 @
                                            compile functionEnvironment (x :: environment)  expression2 @
                                            [ISWAP]                                 @
                                            [IPOP]
     | EQ (expression1, expression2)     -> compile functionEnvironment environment         expression1 @
                                            compile functionEnvironment ("" :: environment) expression2 @
                                            [IEQ]
     | IF (expression1, expression2, expression3)    -> let label2 = newLabel()
                                                        let labele = newLabel()
                                                        compile functionEnvironment environment expression1 @
                                                        [IJMPIF label2]                 @
                                                        compile functionEnvironment environment expression3 @
                                                        [IJMP labele]                   @
                                                        [ILAB label2]                   @
                                                        compile functionEnvironment environment expression2 @
                                                        [ILAB labele]
     |CALL (funkshun, expression)        -> let returnLabel = newLabel()
                                            let functionLabel = lookup funkshun functionEnvironment
                                            compile functionEnvironment environment expression @
                                            [ICALL functionLabel] @
                                            [ILAB returnLabel]    @
                                            [ISWAP]               @
                                            [IPOP];;

//5.5.3 compiling the callee
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
   
*)