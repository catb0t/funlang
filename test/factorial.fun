let
    -- factorial x = if x then factorial (x-1) else 1

    factorial n = 
        (lambda fact: fact fact n)
        (lambda ft k: if k then k * (ft ft (k-1)) else 1)
    in
    factorial 4

