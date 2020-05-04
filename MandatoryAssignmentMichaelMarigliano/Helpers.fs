module Helpers
//Used in Iterpreter Implementation     
        let rec lookup x = function
              | []                      -> failwith ("unbound: " + x)
              | (y, w) :: environment   -> if x = y then w else lookup x environment
              
//Used in Compiler Implementation
        let mutable labelCounter = 0
        
        let newLabel _ = 
             let this = labelCounter
             labelCounter <- this + 1;
             this
             
        let rec variablePosition x = function
          | []                -> failwith ("unbound: " + x)
          | y :: environment  -> if x = y then 0 else 1 + variablePosition x environment