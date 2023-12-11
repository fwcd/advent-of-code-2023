import System.Environment (getArgs)

import Debug.Trace

last :: Data a => [a] -> a
last (_ ++ [x]) = x

diffs :: [Int] -> [Int]
diffs xs = zipWith (-) (drop 1 xs) xs

extrapolate :: [Int] -> Int
extrapolate xs | all (== 0) xs = 0
               | otherwise     = last xs + extrapolate (diffs xs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      raw <- readFile path
      -- We need to read the input strictly since `last`'s functional pattern
      -- introduces nondeterminism, which cannot be used together with lazy IO
      -- (we would get bogus results and end-of-stream errors).
      let input = (read <$>) . words <$> (lines $!! raw)
          next = extrapolate <$> input
          part1 = foldr (+) 0 next
      putStrLn $ "Part 1: " ++ show part1
    _ -> putStrLn "Usage: day09 <input>"
