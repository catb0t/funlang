module Main where

import qualified Data.Map as Map

import Text.Parsec
import FunLang.Parser.Parser
import FunLang.Parser.Desugar
import FunLang.Parser.Symbols
import FunLang.Interpreter.Interpreter
import FunLang.Interpreter.Values

add_fun ((IntegerValue x):(IntegerValue y):[]) = IntegerValue (x+y)
add_fun _ = error "Invalid parameters for add"

builtins = Map.fromList [
    ("0", IntegerValue 0),
    ("1", IntegerValue 1),
    ("+", FunctionValue (BuiltInFunction 2 add_fun)),
    ("add", FunctionValue (BuiltInFunction 2 add_fun))
    ]

main = do
--    args <- getArgs
--    print args
    loop
    where
    loop = do
        line <- getLine
        if null line
            then return ()
            else do
                case (parse parser "" line) of
                    Left err -> do
                        putStrLn "Error at "
                        print err
                    Right ast -> do
                        print ast
                        let dsg = desugar (SymbolTable [] []) ast
                        print dsg
                        let value = evaluate [builtins] dsg
                        print value
                loop

