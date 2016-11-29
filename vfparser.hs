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

data Coordinate = Coord { x :: Int
                        , y :: Int
                        }
instance Show Coordinate where
  show (Coord x y) = "(" ++ (show x) ++ "," ++ (show y) ++ ")"

data Vector  = Vector { i :: Int
                      , j :: Int
                      } deriving (Show)

opify :: Char -> Op
opify c = Op $ case c of
                  '+' -> (+)
                  '-' -> (-)
                  '*' -> (*)
                  -- '/' -> quot

-- returns f(x, y)
functionify :: Expr -> (Int -> Int -> Int)
functionify (Var c) = (\x _ -> x)
functionify (Phrase (Op op) a b) = (\x y -> (p x y) `op` (q x y))
  where
    p = functionify a
    q = functionify b

getVectors:: (Coordinate -> Vector) -> [Coordinate] -> [Vector]
getVectors = map

applyToCoord :: (Int -> Int -> Int) -> Coordinate -> Int
applyToCoord f (Coord x y) = f x y

getCoordList :: (Int, Int) -> (Int, Int) -> Int -> [Coordinate]
getCoordList (xMin, xMax) (yMin, yMax) spacing =
  Coord <$> [xMin, (xMin + spacing)..xMax] <*> [yMin, (yMin + spacing) .. yMax]

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

-- main :: IO ()
