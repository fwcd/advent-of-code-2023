{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields, UndecidableInstances  #-}
{-# LANGUAGE QuantifiedConstraints #-}

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (readFile')
import Data.List (dropWhileEnd)
import Data.Char (isSpace)
import Data.Maybe (mapMaybe)

-- | Trims whitespace from the start/end.
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Splits on the given value.
split :: Eq a => a -> [a] -> [[a]]
split x (y:ys) | x == y    = [] : split x ys
               | otherwise = let (y':ys') = split x ys
                             in (y : y') : ys'
split _ []                 = [[]]

-- | Finds all unordered pairs.
pairs :: [a] -> [(a, a)]
pairs (x:xs) = ((x,) <$> xs) ++ pairs xs
pairs [] = []

data Vec2 a = Vec2 { x :: a, y :: a }
  deriving (Show, Eq, Functor)

data Vec3 a = Vec3 { x :: a, y :: a, z :: a }
  deriving (Show, Eq, Functor)

data Rect2 a = Rect2 { topLeft :: Vec2 a, bottomRight :: Vec2 a }
  deriving (Show, Eq, Functor)

data Hailstone a = Hailstone { pos :: a, vel :: a }
  deriving (Show, Eq, Functor)

type Hailstone2 = Hailstone (Vec2 Float)
type Hailstone3 = Hailstone (Vec3 Float)

class Functor v => Vec v where
  (.+.) :: Num a => v a -> v a -> v a

  neg :: Num a => v a -> v a
  neg = fmap negate

  (.-.) :: Num a => v a -> v a -> v a
  (.-.) v w = v .+. (neg w)

  (*.) :: Num a => a -> v a -> v a
  (*.) l = fmap (l *)

  (.*) :: Num a => v a -> a -> v a
  (.*) = flip (*.)

instance Vec Vec2 where
  v .+. w = Vec2 (v.x + w.x) (v.y + w.y)

instance Vec Vec3 where
  v .+. w = Vec3 (v.x + w.x) (v.y + w.y) (v.z + w.z)

-- | Creates a vector with both components set to the given value.
both :: a -> Vec2 a
both x = Vec2 x x

-- | Projects the given Vec3 to the xy-plane, i.e. a Vec2.
projectXY :: Vec3 a -> Vec2 a
projectXY v = Vec2 v.x v.y

-- | Checks whether the given rectangle contains the given point.
inRect :: Ord a => Rect2 a -> Vec2 a -> Bool
inRect r v = v.x >= r.topLeft.x && v.x <= r.bottomRight.x
          && v.y >= r.topLeft.y && v.y <= r.bottomRight.y

-- | Computes the 2x2 determinant from the given columns.
det2 :: Vec2 Float -> Vec2 Float -> Float
det2 i j = i.x * j.y - i.y * j.x

-- | Computes the 2D intersection between the given two hailstones using Cramer's rule.
-- See https://math.stackexchange.com/questions/406864/intersection-of-two-lines-in-vector-form
intersect2 :: Hailstone2 -> Hailstone2 -> Maybe (Vec2 Float)
intersect2 a b | abs d > 0.00001 && ta >= 0 && tb >= 0 = Just (a.pos .+. (ta *. a.vel))
               | otherwise                             = Nothing
  where
    d  = det2 a.vel (neg b.vel)
    dp = b.pos .-. a.pos
    ta = det2 dp (neg b.vel) / d
    tb = det2 a.vel dp / d

-- | Parsea a Vec3 from the given string.
parseVec3 :: String -> Maybe (Vec3 Float)
parseVec3 raw = do
  [x, y, z] <- Just $ read . trim <$> split ',' raw
  Just $ Vec3 x y z

-- | Parses a hailstone from the given string.
parseHailstone :: String -> Maybe Hailstone3
parseHailstone raw = do
  [pos, vel] <- mapM parseVec3 . map trim $ split '@' raw
  Just $ Hailstone pos vel

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      raw <- readFile' path
      let input  = mapMaybe parseHailstone (lines raw)
          xings  = mapMaybe (uncurry intersect2) (pairs (fmap projectXY <$> input))
          bounds = Rect2 (both 200000000000000) (both 400000000000000)
          part1  = length (filter (inRect bounds) xings)
      putStrLn $ "Part 1: " ++ show part1
    _ -> do
      putStrLn "Usage: day24 <path to input>"
      exitFailure
