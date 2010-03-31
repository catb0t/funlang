module FunLang.Parser.Lexer where

import Text.Parsec
import qualified Text.Parsec.Token as Token

opChar = oneOf "+-/*=:"

keywords = ["if", "then", "elif", "else", "let", "in", "lambda"]

langdef =
    Token.LanguageDef
        "{-" "-}" "--" True  -- comments
        alphaNum (alphaNum <|> oneOf "_")   -- identifiers (start, rest)
        opChar opChar   -- operators (first, rest)
        keywords -- keywords
        ["=", ":"] -- reserved operators
        True -- case sensitive

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser langdef

whiteSpace = Token.whiteSpace lexer
identifier = Token.identifier lexer
parens = Token.parens lexer
operator = Token.operator lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
commaSep1 = Token.commaSep1 lexer
