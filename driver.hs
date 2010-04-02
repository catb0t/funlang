module Main where

import qualified Data.Map as Map
import System.Environment (getArgs)

import Text.Parsec
import FunLang.Parser.Parser
import FunLang.Parser.Desugar
import FunLang.Parser.Symbols
import FunLang.Interpreter.Interpreter
import FunLang.Interpreter.Values

add_fun ((IntegerValue x):(IntegerValue y):[]) = IntegerValue (x+y)
add_fun _ = error "Invalid parameters for add"

neg_fun ((IntegerValue x):[]) = IntegerValue (-x)
neg_fun _ = error "Invalid parameters for add"

builtins = Map.fromList [
    ("0", IntegerValue 0),
    ("1", IntegerValue 1),
    ("+", FunctionValue (BuiltInFunction 2 add_fun)),
    ("add", FunctionValue (BuiltInFunction 2 add_fun)),
    ("neg", FunctionValue (BuiltInFunction 1 neg_fun))
    ]

output (Left err) = do
    putStrLn "Error at "
    print err
output (Right ast) = do
    print ast
    let dsg = desugar (SymbolTable [] []) ast
    print dsg
    let value = evaluate [builtins] dsg
    print value

repl = do
    line <- getLine
    if null line
        then return ()
        else do
            output (parse parser "" line)
            repl

main = do
    args <- getArgs
    case args of
        [] -> repl
        (file:_) -> do
            source <- readFile file
            output (runParser parser () file source)


