module VFParser where
import Text.Parsec hiding ((<|>), many)
import Text.Parsec.String (Parser)
import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Data.Char (isLetter, isDigit)

data Op = Op (Int -> Int -> Int)
instance Show Op where
  show _ = "op"

data Expr = Var Char
          | Phrase Op Expr Expr
          -- | Int
          -- | Parens SimpleExpr
            deriving (Show)

opify :: Char -> Op
opify c = Op $ case c of
                  '+' -> (+)
                  '-' -> (-)
                  '*' -> (*)
                  -- '/' -> quot

-- Parsing
varP :: Parser Expr
varP = Var <$> oneOf "xy"

opP :: Parser Op
opP = opify <$> oneOf "+-*"

phraseP :: Parser Expr
phraseP = do
  v1  <- phraseP
  o   <- opP
  v2  <- phraseP
  return $ Phrase o v1 v2

exprP :: Parser Expr
exprP = (try phraseP) <|> varP

parseExpr :: String -> Either ParseError Expr
parseExpr = parse exprP ""

-- getArguments :: IO ()
-- getArguments = do
--   putStrLn "first function?"
--   putStrLn "second function?"
--   putStrLn ""

-- main :: IO ()
