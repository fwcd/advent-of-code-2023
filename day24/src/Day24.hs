{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields, UndecidableInstances, TypeApplications, NoFieldSelectors  #-}

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (readFile')
import Data.List (dropWhileEnd)
import Data.Char (isSpace)
import Data.Maybe (mapMaybe)
import Control.Monad (forM_)
import Data.Either (fromRight)

-- | Performs an assertion within the Either monad.
assert :: Bool -> e -> Either e ()
assert True  _ = Right ()
assert False e = Left e

-- | Unwraps an Either with a string error.
fromRight' :: Show a => Either a b -> b
fromRight' (Left e)  = error $ show e
fromRight' (Right x) = x

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

-- | A linear system of equations of the form ax = b
data LinearSystem a = LinearSystem { a :: [[a]], b :: [a] }
  deriving (Show, Eq, Functor)

data Hailstone a = Hailstone { pos :: a, vel :: a }
  deriving (Show, Eq, Functor)

type Hailstone2 = Hailstone (Vec2 Double)
type Hailstone3 = Hailstone (Vec3 Double)

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

  (./) :: Fractional a => v a -> a -> v a
  (./) v l = fmap (/ l) v

instance Vec Vec2 where
  v .+. w = Vec2 (v.x + w.x) (v.y + w.y)

instance Vec Vec3 where
  v .+. w = Vec3 (v.x + w.x) (v.y + w.y) (v.z + w.z)

instance Vec [] where
  (.+.) = zipWith (+)
  (.-.) = zipWith (-)

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
det2 :: Vec2 Double -> Vec2 Double -> Double
det2 i j = i.x * j.y - i.y * j.x

-- | Computes the 2D intersection between the given two hailstones using Cramer's rule.
-- See https://math.stackexchange.com/questions/406864/intersection-of-two-lines-in-vector-form
intersect2 :: Hailstone2 -> Hailstone2 -> Maybe (Vec2 Double)
intersect2 a b | abs d > 0.00001 && ta >= 0 && tb >= 0 = Just (a.pos .+. (ta *. a.vel))
               | otherwise                             = Nothing
  where
    d  = det2 a.vel (neg b.vel)
    dp = b.pos .-. a.pos
    ta = det2 dp (neg b.vel) / d
    tb = det2 a.vel dp / d

-- | Transforms a linear system to row-echolon form.
rowEcholonForm :: Fractional a => LinearSystem a -> LinearSystem a
rowEcholonForm sys = rowEcholonForm' 0 sys
  where rowEcholonForm' i sys | i < n     = let (ra:ras) = sys.a
                                                (rb:rbs) = sys.b
                                                rx       = ra !! i
                                                ra'      = ra ./ rx
                                                rb'      = rb / rx
                                                rxs      = (!! i) <$> ras
                                                ras'     = zipWith (\r rx -> r .-. (ra' .* rx)) ras rxs
                                                rbs'     = zipWith (\r rx -> r - (rb' * rx)) rbs rxs
                                                sys'     = rowEcholonForm' (i + 1) $ LinearSystem ras' rbs'
                                            in LinearSystem (ra':sys'.a) (rb':sys'.b)
                              | otherwise = sys
        n        = length sys.a

-- | Flips the matrix/vector horizontally and vertically.
flipLinearSystem :: LinearSystem a -> LinearSystem a
flipLinearSystem sys = LinearSystem (reverse (reverse <$> sys.a)) (reverse sys.b)

-- | Trasnforms a linear system to reduced row echolon form.
reducedRowEcholonForm :: Fractional a => LinearSystem a -> LinearSystem a
reducedRowEcholonForm = flipLinearSystem . rowEcholonForm . flipLinearSystem . rowEcholonForm

-- | Solves a linear system using Gaussian elimination.
solveLinearSystem :: Fractional a => LinearSystem a -> Either String [a]
solveLinearSystem sys = do
  -- Input validation
  let n = length sys.a
  assert (length sys.b == n) $ "Matrix a must have same numbers of rows as vector b"
  forM_ sys.a $ \row -> do
    assert (length row == n) "Matrix a must be quadratic"
  
  let rref = reducedRowEcholonForm sys
  Right rref.b

-- | Parsea a Vec3 from the given string.
parseVec3 :: String -> Maybe (Vec3 Double)
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

      let input               = mapMaybe parseHailstone (lines raw)
          xings               = mapMaybe (uncurry intersect2) (pairs (fmap projectXY <$> input))
          bounds              = Rect2 (both 200000000000000) (both 400000000000000)
          part1               = length (filter (inRect bounds) xings)
          p i                 = (input !! (i - 1)).pos
          v i                 = (input !! (i - 1)).vel
          [px,py,pz,vx,vy,vz] = map (round @_ @Integer) . fromRight' $ solveLinearSystem @Double LinearSystem
          --                    Unknowns:  px                 py                 pz                 vx                 vy                 vz
                                  { a = [ [(v 2).y - (v 1).y, (v 1).x - (v 2).x, 0                , (p 1).y - (p 2).y, (p 2).x - (p 1).x, 0                ]
                                        , [(v 3).y - (v 1).y, (v 1).x - (v 3).x, 0                , (p 1).y - (p 3).y, (p 3).x - (p 1).x, 0                ]
                                        , [(v 1).z - (v 2).z, 0                , (v 2).x - (v 1).x, (p 2).z - (p 1).z, 0                , (p 1).x - (p 2).x]
                                        , [(v 1).z - (v 3).z, 0                , (v 3).x - (v 1).x, (p 3).z - (p 1).z, 0                , (p 1).x - (p 3).x]
                                        , [0                , (v 2).z - (v 1).z, (v 1).y - (v 2).y, 0                , (p 1).z - (p 2).z, (p 2).y - (p 1).y]
                                        , [0                , (v 3).z - (v 1).z, (v 1).y - (v 3).y, 0                , (p 1).z - (p 3).z, (p 3).y - (p 1).y]
                                        ]
                                  , b = [ (p 1).y * (v 1).x - (p 1).x * (v 1).y + (p 2).x * (v 2).y - (p 2).y * (v 2).x
                                        , (p 1).y * (v 1).x - (p 1).x * (v 1).y + (p 3).x * (v 3).y - (p 3).y * (v 3).x
                                        , (p 1).x * (v 1).z - (p 1).z * (v 1).x + (p 2).z * (v 2).x - (p 2).x * (v 2).z
                                        , (p 1).x * (v 1).z - (p 1).z * (v 1).x + (p 3).z * (v 3).x - (p 3).x * (v 3).z
                                        , (p 1).z * (v 1).y - (p 1).y * (v 1).z + (p 2).y * (v 2).z - (p 2).z * (v 2).y
                                        , (p 1).z * (v 1).y - (p 1).y * (v 1).z + (p 3).y * (v 3).z - (p 3).z * (v 3).y
                                        ]
                                  }

      putStrLn $ "Part 1: " ++ show part1
      putStrLn $ "Part 2: " ++ show (px + py + pz) ++ " (p = " ++ show [px, py, pz] ++ ", v = " ++ show [vx, vy, vz] ++ ")"
    _ -> do
      putStrLn "Usage: day24 <path to input>"
      exitFailure
