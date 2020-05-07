#load "VirtualMachine.fs"
open VM

let mutable labelCounter = 0

let newLabel _ = 
 let this = labelCounter
 labelCounter <- this + 1;
 this
 
let rec varPos x = function
| []                -> failwith ("unbound: " + x)
| y :: env  -> if x = y then 0 else 1 + varPos x env

let rec comp funcs env = function
 | INT i               -> [IPUSH i]
 | ADD (exp1, exp2)    -> comp funcs env         exp1 @
                                        comp funcs ("" :: env) exp2 @
                                        [IADD]
 | SUB (exp1, exp2)    -> comp funcs env         exp1 @
                                        comp funcs ("" :: env) exp2 @
                                        [ISUB]
 | MUL (exp1, exp2)    -> comp funcs env         exp1 @
                                        comp funcs ("" :: env) exp2 @
                                        [IMUL]
 | DIV (exp1, exp2)    -> comp funcs env         exp1 @
                                        comp funcs ("" :: env) exp2 @
                                        [IDIV]
 | NEG (exp)                  -> comp funcs env (INT(0)) @
                                        comp funcs ("" :: env) exp @
                                        [ISUB]
 | VAR x                             -> [IGET (varPos x env)]
 | LET (x, exp1, exp2) -> comp funcs env         exp1 @
                                        comp funcs (x :: env)  exp2 @
                                        [ISWAP]@
                                        [IPOP]
 | EQ (exp1, exp2)     -> comp funcs env         exp1 @
                                        comp funcs ("" :: env) exp2 @
                                        [IEQ]
 | NEQ (exp1, exp2)    -> comp funcs env (INT(1)) @
                                        comp funcs ("" :: env) (EQ(exp1, exp2)) @
                                        [ISUB]
 | LT (exp1, exp2)     -> comp funcs env         exp1 @
                                        comp funcs ("" :: env) exp2 @
                                        [ILT]
 | LE (exp1, exp2)     -> comp funcs env         exp1 @
                                        comp funcs ("" :: env) exp2 @
                                        [ILE]
 | GT (exp1, exp2)     -> comp funcs env         exp2 @
                                        comp funcs ("" :: env) exp1 @
                                        [ILT]
 | GE (exp1, exp2)     -> comp funcs env         exp2 @
                                        comp funcs ("" :: env) exp1 @
                                        [ILE]
 | IF (exp1, exp2, exp3)    ->  let label2 = newLabel()
                                let labele = newLabel()
                                comp funcs env exp1 @
                                [IJMPIF label2]                 @
                                comp funcs env exp3 @
                                [IJMP labele]                   @
                                [ILAB label2]                   @
                                comp funcs env exp2 @
                                [ILAB labele]
 | AND (exp1, exp2)    -> comp funcs env (INT(2))@
                                        comp funcs ("" :: env) (exp1) @
                                        comp funcs ("" :: env) (exp2) @
                                        [IADD]@
                                        [IEQ]
 | OR (exp1, exp2)    ->  comp funcs env (INT(0)) @ 
                                        comp funcs ("" :: env) (exp1) @
                                        comp funcs ("" :: env) (exp2) @
                                        [IADD] @
                                        [ILT]
 | CALL (func, exp)                 ->  let labr = newLabel()
                                        let labf = lookup func funcs
                                        let loop =
                                            let rec exps exp = 
                                                      match exp with
                                                      | [] -> []
                                                      | e::exp -> 
                                                         comp funcs ("" :: env) e @
                                                         exps exp
                                            exps exp
                                        loop @    
                                        [ICALL labf] @
                                        [ILAB  labr] @
                                        [ISWAP] @
                                        [IPOP]
                                        

let compProg (funcs, exp) = 
        let funcEnv = List.map (fun (f, _) -> (f, newLabel())) funcs
        let rec compFuncs = function
          | []                               -> comp funcEnv [] exp @
                                                [IHALT]
          | (f, (x, e)) :: funcs             -> let labelf = lookup f funcEnv
                                                compFuncs funcs      @
                                                [ILAB labelf]@
                                                comp funcEnv ["";x] e  @
                                                [ISWAP]@
                                                [IRETN]
        compFuncs funcs

                                        
let compAdd = comp [] [] (ADD(INT 1, INT 1)) 
let Add = execProg compAdd [] 
let compSub = comp [] [] (SUB(INT 1, INT 0))
let Sub = execProg compSub [] 
let compMul = comp [] [] (MUL(INT 1, INT 1))
let Mul = execProg compMul [] 
let compDiv = comp [] [] (DIV(INT 1, INT 1)) 
let Div = execProg compDiv [] 
let compNeg = comp [] [] (NEG(INT 1))
let Neg = execProg compNeg [] 
let compEq = comp [] [] (EQ(INT 1, INT 2)) 
let Eq = execProg compEq [] 
let compNeq = comp [] [] (NEQ(INT 10, INT 10)) 
let Neq = execProg compNeq [] 
let compLet = comp [] [] (LET("y", INT(5), SUB(VAR "y", INT 4)))
let Let = execProg compLet [] 
let compGe = comp [] [] (GE(INT(5), INT(6)))
let Ge = execProg compGe [] 
let compGt = comp [] [] (GT(INT(1), INT(3)))
let Gt = execProg compGt [] 
let compLe = comp [] [] (LE(INT(2), INT(1)))
let Le = execProg compLe []
let compLt = comp [] [] (LT(INT(1), INT(1)))
let Lt = execProg compLt [] 
let compIfAnd = comp [] [] (IF(AND(EQ(INT(5), INT(5)),EQ(INT(4),INT(4))),INT(1),INT(0)))
let IfAnd = execProg compIfAnd [] 
let compIfOr = comp [] [] (IF(OR(EQ(INT(5), INT(5)),EQ(INT(3),INT(4))),INT(1),INT(0)))
let IfOr = execProg compIfOr [] 
//from doc
let compFunction = compProg([("Add2", ("x", ADD(INT 2, VAR "x")))], (CALL("Add2", [INT 5])))
let Function = execProg compFunction []

    
