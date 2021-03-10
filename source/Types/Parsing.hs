{-|

Copyright: © 2021 moretrim
Licence:   GPL-3.0-only

Common items relating to Victoria II file parsing.

|-}
module Types.Parsing
    ( Parser

    , lineComment, ws, ws'
    , lexeme, lexeme'
    , symbol, symbol'
    , keyword, keyword'
    , rawIdentifier, rawIdentifier'
    , rawQuotedIdentifier, rawQuotedIdentifier'
    , rawUnquotedIdentifier, rawUnquotedIdentifier'

    , raw
    , commented', many'
    , identifier, identifier'
    , scalar, scalar'
    , decimal, decimal'
    , date, date'
    , block
    , pair
    , numericPair
    , entry
    , section

    , blob, pairish, blockish

    -- | Take over the module, taking care of clashes
    , module Types
    , module Control.Applicative.Permutations
    , module Control.Monad.Combinators.NonEmpty
    , module Text.Megaparsec
    , module Text.Megaparsec.Char
    ) where

import qualified Data.Text as Text

import Control.Applicative.Permutations   (runPermutation, toPermutation, toPermutationWithDefault)
import Control.Monad.Combinators.NonEmpty (some)

import Text.Megaparsec hiding             (some)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

import Types hiding                       (many, some)

type Parser = Parsec Void Text

------------
-- Lexing --
------------

lineComment :: MonadParsec errors Text parser
            => parser Text
lineComment = Text.cons <$> char '#' <*> takeWhileP (Just "comment character") (/= '\n')

-- | Lax whitespace, i.e. also consumes comments.
ws :: MonadParsec errors Text parser
   => parser ()
ws = Lex.space space1 (void lineComment) {- no block comments #-} empty

-- | Strict whitespace, consumes at most one whitespace character & does not consume comments.
ws' :: MonadParsec errors Text parser
    => parser ()
ws' = Lex.space (void spaceChar) {- no comments of any kind #-} empty empty

-- Convention: stricter lexers (e.g. that rely on `ws'`, or entirely ignore whitespace) are quoted.

lexeme, lexeme' :: MonadParsec errors Text parser
                => parser item -> parser item
lexeme  = Lex.lexeme ws
lexeme' = Lex.lexeme ws'

{-

Verbatim lexemes differ in when they are case-sensitive and when they are not. Generally, the
following holds:

- syntactic parts of PDS script are case-insensitive
- parts which refer to an item are not, especially when they’re moddable (which comes into play when
  connecting that item to a localisation key and its translations)

Example PDS script:

    # All identical triggers, even though conventionally logical negation
    # is cased as `NOT` and tag as `tag`.
    #
    # Because the target of `tag` is a moddable item (i.e. a country in this case),
    # it is case sensitive. (Presumably anyway.)
    NOT = { tag = ENG }
    not = { tag = ENG }
    NOT = { TAG = ENG }

-}

-- | Verbatim, case-sensitive lexemes.
symbol, symbol' :: MonadParsec errors Text parser
                => Text -> parser Text
symbol  = Lex.symbol ws
symbol' = Lex.symbol ws'

-- | Verbatim, case-insensitive lexemes.
keyword, keyword' :: MonadParsec errors Text parser
                  => Text -> parser Text
keyword  = Lex.symbol' ws
keyword' = Lex.symbol' ws'

rawIdentifier, rawQuotedIdentifier, rawUnquotedIdentifier,
    rawIdentifier', rawQuotedIdentifier', rawUnquotedIdentifier' :: MonadParsec errors Text parser
                                                                 => parser Text

rawIdentifier' = rawQuotedIdentifier' <|> rawUnquotedIdentifier'

rawQuotedIdentifier' = parser <?> descr
  where
    parser = between quote quote $
        takeWhile1P (Just "quoted identifier character") (/= '"')
    descr = "a quoted identifer (e.g. `\"a quoted identifier\"`)"
    quote = char '"'

rawUnquotedIdentifier' = parser <?> descr
  where
    parser = takeWhile1P (Just "identifier character") validIdentifierChar
    descr = "an unquoted identifier (e.g. `unquoted-identifier`)"
    validIdentifierChar = nonSyntactic

rawIdentifier         = lexeme rawIdentifier'
rawQuotedIdentifier   = lexeme rawQuotedIdentifier'
rawUnquotedIdentifier = lexeme rawUnquotedIdentifier'

-- | Non-syntactic character predicate.
nonSyntactic :: Char -> Bool
nonSyntactic c = not (isSpace c) && c `notElem` ("\"#={};,()!&" :: String)

-- | Non-syntactic consumer.
nonSyntax :: MonadParsec errors Text parser
          => parser Text
nonSyntax = takeWhile1P (Just "non-syntactic character") nonSyntactic
-------------
-- Parsing --
-------------

-- | A variant of Megaparsec’s `match`, focussing on just the matched text.
raw :: MonadParsec errors Text parser
    => parser any -> parser Text
raw p = fst <$> match p

-- | Parse an item or a comment.
commented' :: MonadParsec errors Text parser
           => parser item -> parser (Interspersed item)
commented' item = try (lexeme' $ Comment <$> lineComment) <|> (Parsed <$> item)

-- | Parse items interspersed with comments, preserving the comments. NOTE: this functionality is
-- limited.
many' :: MonadParsec errors Text parser
      => parser item -> parser [Interspersed item]
many' = many . commented'

identifier, quotedIdentifier, unquotedIdentifier,
    identifier', quotedIdentifier', unquotedIdentifier' :: MonadParsec errors Text parser
                                                        => parser Identifier

identifier = quotedIdentifier <|> unquotedIdentifier
quotedIdentifier   = QuotedIdentifier <$> rawQuotedIdentifier
unquotedIdentifier = UnquotedIdentifier <$> rawUnquotedIdentifier

identifier' = quotedIdentifier' <|> unquotedIdentifier'
quotedIdentifier'   = QuotedIdentifier <$> rawQuotedIdentifier'
unquotedIdentifier' = UnquotedIdentifier <$> rawUnquotedIdentifier'

-- | PDS script placeholder: more or less anything that’s not whitespace/comments/syntax.
scalar, scalar' :: MonadParsec errors Text parser
                => parser Text
scalar  = rawQuotedIdentifier  <|> lexeme  nonSyntax
scalar' = rawQuotedIdentifier' <|> lexeme' nonSyntax

-- | Parse a PDS script numeric value.
decimalP :: MonadParsec errors Text parser
         => (parser Decimal -> parser Decimal)
         -> parser ()
         -> parser Decimal
decimalP lexP wsP = lexP (Lex.signed wsP parser) <?> "numeric value"
  where
    parser = try fractional <|> (toDecimal <$> integral)

    integral   = Lex.decimal
    fractional = integerPart >>= fractionalPart

    -- N.b. not appropriate for fractional parts. We want to preserve the input precision, and the
    -- whole of `Num` etc. is not setup for that. The goal is that a parsed `"1.0"` is really stored
    -- as `1.0`, and not `1`.
    toDecimal = fromInteger @Decimal

    integerPart        = fromMaybe 0 <$> optional integral
    fractionalPart int = fromDigits int <$ decimalSeparator <*> takeWhile1P (Just "digit") isDigit

    decimalSeparator = symbol "."
    fromDigits int = fromTally int . Text.foldl' tally (0 :: Word8, 1 :: Integer, 0 :: Integer)
    tally (!places, !exp', !mag) d = (succ places, exp' * 10, 10 * mag + digit d)
    -- This is where we are careful about storing precision, by using specific `Decimal` machinery
    -- (that we know won’t perform normalisation) instead of the more general numeric classes.
    fromTally int (!places, !exp', !mag) = Decimal places $ exp' * int + mag
    digit '0' = 0
    digit '1' = 1
    digit '2' = 2
    digit '3' = 3
    digit '4' = 4
    digit '5' = 5
    digit '6' = 6
    digit '7' = 7
    digit '8' = 8
    digit '9' = 9
    digit  _  =
        error "Parsing.decimal.digit: non-exhaustive pattern match, decimal digit was expected"

decimal, decimal' :: MonadParsec errors Text parser
                  => parser Decimal
decimal  = decimalP lexeme  ws
decimal' = decimalP lexeme' ws'

-- | Parse a PDS script date (e.g. “1836.1.1”). Not actually implemented.
date, date' :: MonadParsec errors Text parser
            => parser Text
date  = scalar
date' = scalar'

-- | `{ body }`
block :: MonadParsec errors Text parser
      => parser body -> parser body
block = between (symbol "{") (symbol "}")

-- | `key = value` pair
pair :: MonadParsec errors Text parser
     => parser key -> parser value -> parser (key, value)
pair key value = (,) <$> key <* symbol "=" <*> value

-- | `key = value` pair, where key is an identifier and value is numeric.
numericPair :: MonadParsec errors Text parser
            => parser (Identifier, Decimal)
numericPair = pair identifier decimal

-- | `literal = value` pair, where the key is syntactically expected. Case-insensitive in the
-- literal, to reflect the behaviour of the game.
entry :: MonadParsec errors Text parser
      => Text -> parser value -> parser value
entry key value = snd <$> pair key' value
  where
    key' = keyword key <?> [i|entry key ‘${ Text.unpack key }’|]

-- | `key = { body }` pair with a verbatim, syntactic key (see `entry`) aka a section.
section :: MonadParsec errors Text parser
        => Text -> parser body -> parser body
section key body = entry key (block body)

-----------------------------------------
-- Unstructured parsers of last resort --
-----------------------------------------

-- | Unstructured `a = { b = { 0 1 2 } c }` PDS script. Recognises any one item at the top-level,
-- including a scalar.
blob :: MonadParsec errors Text parser
     => parser Text
blob = try blockish <|> try pairish <|> scalar

-- | Unstructured PDS script. Only recognises a top-level pair.
pairish :: MonadParsec errors Text parser
        => parser Text
pairish = raw $ pair scalar scalar

-- | Unstructured PDS script. Only recognises a top-level block.
blockish :: MonadParsec errors Text parser
         => parser Text
blockish = raw $ pair scalar (block $ many blob)
