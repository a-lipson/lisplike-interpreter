{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PST.Parser where

import           Control.Monad (void)
import           Data.Char     (isSpace)
import           PST.Token
import           PST.Types

data ParserError
  = UnexpectedEOF
  | UnexpectedChar Char
  | UnexpectedToken Token
  | UnmatchedParen Pos
  | LexerError String Pos
  deriving (Show)

data Pos = Pos
  { line :: Int
  , col  :: Int
  }
  deriving (Show, Eq)

data ParserState = ParserState
  { input :: String
  , pos   :: Pos
  }

newtype Parser a = Parser (ParserState -> Either ParserError (a, ParserState))
  deriving (Functor) -- , Applicative, Monad)

runParser :: Parser a -> ParserState -> Either ParserError (a, ParserState)
runParser (Parser f) = f

-- initialPos :: Pos
-- initialPos = Pos 1 1
--
-- advancePos :: Pos -> Char -> Pos
-- advancePos (Pos l _) '\n' = Pos (l + 1) 1
-- advancePos (Pos l c) _    = Pos l (c + 1)
--
-- item :: Parser Char
-- item = Parser $ \s -> case input s of
--   []     -> Left UnexpectedEOF
--   c : cs -> pure (c, ParserState cs (advancePos (pos s) c))
--
-- satisfy :: (Char -> Bool) -> Parser Char
-- satisfy p = Parser $ \s -> case runParser item s of
--   Left err -> Left err
--   Right (c, s')
--     | p c -> pure (c, s')
--     | otherwise -> Left (UnexpectedChar c)
--
-- many :: Parser a -> Parser [a]
-- many p = some p <|> pure []
--
-- some :: Parser a -> Parser [a]
-- some p = do
--   x <- p
--   xs <- many p
--   return (x : xs)
--
-- -- token parsers
--
-- skipWhitespace :: Parser ()
-- skipWhitespace = void $ many (satisfy isSpace)
--
-- parseSymbol :: Parser Token
-- parseSymbol = do
--   p <- pos
--   chars <- some (satisfy (\c -> not (isSpace c || c `elem` "();")))
--   return $ Token (SymbolT (concat chars)) (line p) (col p)
--
-- parseToken :: Parser Token
-- parseToken = do
--   skipWhitespace
--   p <- pos
--   (satisfy (== '(') >> return (Token OpenParen (line p) (col p)))
--     <|> (satisfy (== ')') >> return (Token CloseParen (line p) (col p)))
--     <|> parseSymbol
--     <|> (eof >> return (Token EOFT (line p) (col p)))
--
-- parseSExpr :: Parser SExpr
-- parseSExpr = do
--   token <- parseToken
--   case token of
--     Token (SymbolT s) -> return $ Symbol s
--     Token OpenParen   -> parseList
--
