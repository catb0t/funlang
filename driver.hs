module Main where

import Text.Parsec
import FunLang.Parser.Parser
import FunLang.Parser.Desugar
import FunLang.Parser.Symbols

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
                    Right x -> do
                        print x
                        print (desugar (SymbolTable [] []) x)
                loop

