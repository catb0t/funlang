{-
    fibonacci x =
        if x 
        then
            if x + neg 1 then fibonacci (x + neg 1) + fibonacci (x + neg 2) else 1
        else 0

    fibonacci_iter x =
        let iter n a b = if n then iter (n + neg 1) b (a+b) else a
        in iter x 0 1
-}

let
    fibonacci x =
        (lambda fib: fib fib x)
        (lambda fib n: 
            if n
            then
                if n + neg 1 then fib fib (n + neg 1) + fib fib (n + neg 2) else 1
            else 0)
    ,
    fibonacci_iter x =
        (lambda iter: iter iter x 0 1)
        (lambda iter n a b: if n then iter iter (n + neg 1) b (a+b) else a)
in
    fibonacci 7 + fibonacci_iter 8


