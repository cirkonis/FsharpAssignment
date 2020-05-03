module Helpers
        
        let rec lookup x = function
              | []                      -> failwith ("unbound: " + x)
              | (y, w) :: environment   -> if x = y then w else lookup x environment
              

