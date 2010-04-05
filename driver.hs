module Main where

import qualified Data.Map as Map
import System.Environment (getArgs)

import Text.Parsec
import FunLang.Parser.Parser
import FunLang.Parser.Desugar
import FunLang.Interpreter.Interpreter
import FunLang.Interpreter.Values
import FunLang.Intermediate.SimpleRewrite
import qualified FunLang.Parser.Pretty as PrettyAST
import qualified FunLang.Intermediate.Pretty as PrettyDsg
import qualified FunLang.Interpreter.Pretty as PrettyValue

binary_fun op ((IntegerValue x):(IntegerValue y):[]) = IntegerValue (op x y)
binary_fun _ args = error ("Invalid parameters for binary function" ++ show args)

unary_fun op ((IntegerValue x):[]) = IntegerValue (op x)
unary_fun _ args = error ("Invalid parameters for unary function: " ++ show args)

add_fun = binary_fun (+)
mul_fun = binary_fun (*)
neg_fun = unary_fun (\x -> -x)

builtins = Map.fromList [
    ("+", FunctionValue (BuiltInFunction 2 add_fun)),
    ("*", FunctionValue (BuiltInFunction 2 mul_fun)),
    ("add", FunctionValue (BuiltInFunction 2 add_fun)),
    ("neg", FunctionValue (BuiltInFunction 1 neg_fun))
    ]

output (Left err) = do
    putStrLn "Error at "
    print err
output (Right ast) = do
    putStrLn "===== AST ====="
    putStrLn (PrettyAST.pprint ast)
    putStrLn "===== Desugared ====="
    let dsg = desugar () ast
    putStrLn (PrettyDsg.pprint dsg)
    putStrLn "===== Rewritten ====="
    let rew = rewrite dsg
    putStrLn (PrettyDsg.pprint rew)
    putStrLn "===== Evaluated ====="
    let value = evaluate [builtins] rew
    putStrLn (PrettyValue.pprint value)

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


