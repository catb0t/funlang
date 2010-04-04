let
    even n = if n then (odd (n+neg 1)) else 1,
    odd n = if n then (even (n+neg 1)) else 0
in
    even 12 + odd 12
{-
let
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


(lambda even odd: -- wrapper
    (lambda even odd: even 12 + odd 12) -- body
        (even even odd) -- selectors
        (odd even odd)
    )
    (lambda even odd n: if n then (odd even odd (n+neg 1)) else 1) -- rewritten definitions
    (lambda even odd n: if n then (even even odd (n+neg 1)) else 0)

let 
-- recursive: {even: [even, odd, decr], odd: [even, odd, decr], decr: [sub] }
-- depends: {even: [even, odd, decr, sub], odd: [even, odd, decr, sub], decr: [sub] }
-- scopes: {[sub]: [decr], [even, odd, decr, sub]: [even, odd]}
-- simple: [x]
    even n = if n then (odd (decr n)) else 1,
    odd n = if n then (even (decr n)) else 0,
    decr n = sub n 1,
    sub a b = a + neg b,
    x = 12
in
    even x + odd x

(lambda sub: -- wrapper for scope {[sub]: [decr]}
    (lambda decr: -- body for scope {[sub], [decr]} 
        (lambda even odd: -- wrapper for scope {[even, odd, decr, sub]: [even, odd]}, locals: [even, odd]
            (lambda even odd: -- body
                (lambda x: even x + odd x) 12) -- simple
                (even even odd) -- selectors for scope {[even, odd, decr, sub]: [even, odd]}
                (odd even odd)
            )
        (lambda even odd:  -- rewritten definitions for scope {[even, odd, decr, sub]: [even, odd]}
            (lambda n: if n then ((odd even odd) (decr n)) else 1))
        (lambda even odd: 
            (lambda n: if n then ((even even odd) (decr n)) else 0))
        ) -- end wrapper {[even. odd, decr, sub]: [even, odd]}
	(lambda n: sub n 1) -- selectors for scope {[sub]:[decr]}
	) -- end wrapper {[sub]: [decr]}
	(lambda a b: a + neg b)
-}

