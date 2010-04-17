module Main where

import qualified Data.Map as Map

import FunLang.Interpreter.Values
import FunLang.Interpreter.SSA
import FunLang.SSA.SSA

fac = 
    Function ["x"] [
        ("entry",
            BasicBlock [
                ("initial", Const 1)
                ]
                (Branch "x" "loop" "exit"))
        , ("loop",
            BasicBlock [
                ("counter", Phi [("entry", "x"), ("loop", "next")]),
                ("value", Phi [("entry", "initial"), ("loop", "product")]),
                ("product", FunCall "mul" ["counter", "value"]),
                ("one", Const 1),
                ("next", FunCall "sub" ["counter", "one"])
                ]
                (Branch "next" "loop" "exit"))
        , ("exit",
            BasicBlock [
                ("value", Phi [("loop", "product"), ("entry", "initial")])
                ]
                (Return "value"))]
    Map.empty

fac_rec =
    Function ["x"] [
        ("entry",
            BasicBlock
                []
                (Branch "x" "recurse" "exit")
                ),
        ("exit",
            BasicBlock
                [("initial", Const 1)]
                (Return "initial")
                ),
        ("recurse",
            BasicBlock [
                ("one", Const 1),
                ("next", FunCall "sub" ["x", "one"]),
                ("rec", FunCall "fac_rec" ["next"]),
                ("product", FunCall "mul" ["rec", "x"])
                ]
                (Return "product"))]
    Map.empty


wrap f = FunctionValue (BuiltInFunction 2 (\(IntegerValue a : IntegerValue b : []) -> IntegerValue (f a b)))

builtins = Map.fromList [
    ("mul", wrap (*)),
    ("sub", wrap (-)),
    ("fac", FunctionValue (ClosureFunction [builtins] ["x"] fac)),
    ("fac_rec", FunctionValue (ClosureFunction [builtins] ["x"] fac_rec))
    ]

main = do
    print $ simulate [builtins] fac [IntegerValue 5]
    print $ simulate [builtins] fac_rec [IntegerValue 5]


