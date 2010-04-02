let
    fac x = 
        if x
            then fac (x + neg 1)
            else 1
in
    fac 5


