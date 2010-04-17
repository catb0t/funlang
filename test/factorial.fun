let
    factorial x = if x then x * factorial (x-1) else 1,
    fact_iter x =
        let iter count x = if x then iter (x*count) (x-1) else count 
        in iter 1 x

    in
    (factorial 4 + fact_iter 4) / 2


{-
factorial = 
    (lambda fact: fact fact)
    (lambda ft k: if k then k * (ft ft (k-1)) else 1)
    
fact_iter x =
    (lambda iter: iter iter 1 x)
    (lambda iter count x: if x then iter iter (x*count) (x-1) else count)

-}

