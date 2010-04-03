{-
let
    -- even n = if n then (odd (n+neg 1)) else 1
    -- odd n = if n then (even (n+neg 1)) else 0

    isEven x =
        (lambda even odd: even even odd x)
        (lambda ev od n: if n then (od ev od (n+neg 1)) else 1)
        (lambda ev od n: if n then (ev ev od (n+neg 1)) else 0)
    ,
    isOdd x =
        (lambda even odd: odd even odd x)
        (lambda ev od n: if n then (od ev od (n + neg 1)) else 1)
        (lambda ev od n: if n then (ev ev od (n + neg 1)) else 0)
    in
    isEven 12 + isOdd 12

let ev = (lambda ev od n: if n then (od ev od (n+neg 1)) else 1),
    od = (lambda ev od n: if n then (ev ev od (n+neg 1)) else 0)
in
    let even = (lambda even odd: even even odd) ev od,
        odd = (lambda even odd: odd even odd) ev od
    in
        even 12 + odd 12
    
(lambda ev od:
    let even = (lambda even odd: even even odd) ev od,
        odd = (lambda even odd: odd even odd) ev od
    in even 12 + odd 12)
    (lambda ev od n: if n then (od ev od (n+neg 1)) else 1),
    (lambda ev od n: if n then (ev ev od (n+neg 1)) else 0)

-}

(lambda ev od:
    (lambda even odd: even 12 + odd 12)
        ((lambda even odd: even even odd) ev od)
        ((lambda even odd: odd even odd) ev od)
    )
    (lambda ev od n: if n then (od ev od (n+neg 1)) else 1)
    (lambda ev od n: if n then (ev ev od (n+neg 1)) else 0)
    
        
