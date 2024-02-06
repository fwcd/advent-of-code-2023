{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields, UndecidableInstances  #-}
{-# LANGUAGE QuantifiedConstraints #-}

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (readFile')

-- | Finds all unordered pairs.
pairs :: [a] -> [(a, a)]
pairs (x:xs) = ((x,) <$> xs) ++ pairs xs
pairs [] = []

data Vec2 a = Vec2 { x :: a, y :: a }
  deriving (Show, Eq, Functor)

data Vec3 a = Vec3 { x :: a, y :: a, z :: a }
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

-- | Projects the given Vec3 to the xy-plane, i.e. a Vec2.
projectXY :: Vec3 a -> Vec2 a
projectXY v = Vec2 v.x v.y

-- | Computes the 2x2 determinant from the given columns.
det2 :: Vec2 Float -> Vec2 Float -> Float
det2 i j = i.x * j.y - i.y * j.x

-- | Computes the 2D intersection between the given two hailstones using Cramer's rule.
intersect2 :: Hailstone2 -> Hailstone2 -> Maybe (Vec2 Float)
intersect2 a b | d > 0.00001 && r >= 0 && s >= 0 = Just (a.pos .+. (r *. a.vel))
               | otherwise                       = Nothing
  where
    d = det2 a.vel b.vel
    v = b.pos .-. a.pos
    r = det2 (Vec2 1 0) v / d
    s = det2 v (Vec2 0 1) / d

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      raw <- readFile' path
      putStrLn raw
    _ -> do
      putStrLn "Usage: day24 <path to input>"
      exitFailure
