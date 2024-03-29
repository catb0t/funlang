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
    
{-
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


-- simpler version

let
    even n = if n then (odd (n+neg 1)) else 1,
    odd n = if n then (even (n+neg 1)) else 0
in
    even 12 + odd 12


(lambda even odd: -- wrapper
    (lambda even odd: even 12 + odd 12) -- body
        (even even odd) -- selectors
        (odd even odd)
    )
    (lambda even odd: lambda n: if n then (odd even odd (n+neg 1)) else 1) -- rewritten definitions
    (lambda even odd: lambda n: if n then (even even odd (n+neg 1)) else 0)

-}

