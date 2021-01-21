{-|

Copyright: © 2021 moretrim
Licence:   GPL-3.0-only

Common parsing items.

|-}
module Parsing
    ( Parser
    , lineComment, ws, ws'
    , lexeme, lexeme' , symbol, symbol'
    , identifier
    , commented', many'
    , block
    , identifierPair
    , scriptEntry
    ) where

import GHC.Generics                                (Generic)

import Control.Lens

import Data.Functor

import Data.Void
import Data.Char
import Data.String.Here.Interpolated
import qualified Data.Text as Text

import Text.Megaparsec hiding                      (some)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

import Types

type Parser = Parsec Void Text

----------------------------------
-- Victoria II specific parsing --
----------------------------------

-- Lexing --

lineComment :: Parser Text
lineComment = Text.cons <$> char '#' <*> takeWhileP (Just "comment character") (/= '\n')

-- | Lax whitespace, i.e. also consumes comments.
ws :: Parser ()
ws = Lex.space space1 (void lineComment) {- no block comments #-} empty

-- | Strict whitespace, does not consume comments.
ws' :: Parser ()
ws' = Lex.space space1 {- no comments of any knid #-} empty empty

-- Convention: strict lexers (that rely on `ws'`) are quoted.

lexeme, lexeme' :: Parser item -> Parser item
lexeme  = Lex.lexeme ws
lexeme' = Lex.lexeme ws'

symbol, symbol' :: Text -> Parser Text
symbol  = Lex.symbol ws
symbol' = Lex.symbol ws'

-- Parsing --

identifier, quotedIdentifier, unquotedIdentifier :: Parser Identifier

identifier = quotedIdentifier <|> unquotedIdentifier

quotedIdentifier = parser <?> descr
  where
    parser = fmap QuotedIdentifier . lexeme . between quote quote $
        takeWhile1P (Just "quoted identifier character") (/= '"')
    descr = "a quoted identifer (e.g. `\"a quoted identifier\"`)"
    quote = char '"'

unquotedIdentifier = parser <?> descr
  where
    parser = fmap UnquotedIdentifier . lexeme $
        takeWhile1P (Just "identifier character") validIdentifierChar
    descr = "an unquoted identifier (e.g. `unquoted-identifier`)"
    validIdentifierChar c = not (isSpace c) && c `notElem` ("\"#={};,()!&" :: String)

-- | Parse an item or a comment.
commented' :: Parser item -> Parser (Interspersed item)
commented' item = try (lexeme' $ Comment <$> lineComment) <|> (Interspersed <$> item)

-- | Parse items interspersed with comments, preserving the comments. NOTE: this functionality is
-- limited.
many' :: Parser item -> Parser [Interspersed item]
many' = many . commented'

-- | `{ body }`
block :: Parser body -> Parser body
block = between (symbol "{") (symbol "}")

-- | `key = value` pair, where key is an identifier and value will be parsed as raw as possible.
identifierPair :: Parser (Identifier, Text)
identifierPair = do
    key <- identifier
    symbol "="
    value <- takeWhile1P (Just "non-space character") (not . isSpace)
    optional ws
    pure $ (key, value)

-- | `key = { body }`
scriptEntry :: Text -> Parser body -> Parser body
scriptEntry key body = sectionKey *> symbol "=" *> block body
  where
    sectionKey = lexeme (string' key) <?> [i|entry key ‘${ Text.unpack key }’|]
