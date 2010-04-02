let
    cons x y f = f x y,
    car x = x (lambda p q: p)
in
    car (cons 1 0)


