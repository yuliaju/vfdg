module VFLib where
import VFParser

data Coordinate = Coord { x :: Int
                        , y :: Int
                        }
instance Show Coordinate where
  show (Coord x y) = "(" ++ (show x) ++ "," ++ (show y) ++ ")"

data Vector  = Vector { i :: Int
                      , j :: Int
                      } deriving (Show)

-- returns f(x, y)
functionify :: Expr -> (Int -> Int -> Int)
functionify (Var c) = (\x _ -> x)
functionify (Phrase (Op op) a b) = (\x y -> (p x y) `op` (q x y))
  where
    p = functionify a
    q = functionify b

vectorFunctionify :: (Int -> Int -> Int) -> (Int -> Int -> Int) -> (Coordinate -> Vector)
vectorFunctionify b1 b2 = vf
  where
    vf (Coord x y) = Vector (b1 x y) (b2 x y)

getCoords :: (Int, Int) -> (Int, Int) -> Int -> [Coordinate]
getCoords (xMin, xMax) (yMin, yMax) spacing =
  Coord <$> [xMin, (xMin + spacing)..xMax] <*> [yMin, (yMin + spacing) .. yMax]

applyToCoord :: (Int -> Int -> Int) -> Coordinate -> Int
applyToCoord f (Coord x y) = f x y

getVectorField :: (Coordinate -> Vector) -> [Coordinate] -> [(Coordinate, Vector)]
getVectorField f coords = zipWith (\x y -> (x, y)) coords vectors
  where
    vectors = fmap f coords
