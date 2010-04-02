let
    -- even n = if n then (odd (n+neg 1)) else 1
    -- odd n = if n then (even (n+neg 1)) else 0

    isEven x =
        (lambda even odd: even even odd x)
        (lambda ev od n: if n then (od ev od (n+neg 1)) else 1)
        (lambda ev od n: if n then (ev ev od (n+neg 1)) else 0)
    in
    isEven 12

