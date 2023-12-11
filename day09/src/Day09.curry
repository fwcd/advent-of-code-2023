import System.Environment (getArgs)

extrapolate :: [Int] -> Int
extrapolate = extrapolate' . reverse
  where
    extrapolate' :: [Int] -> Int
    extrapolate' xs | all (== 0) xs = 0
                    | otherwise     = head xs + extrapolate' (diffs xs)

    diffs :: [Int] -> [Int]
    diffs xs = zipWith (-) xs (drop 1 xs)

result :: [[Int]] -> Int
result = foldr (+) 0 . (extrapolate <$>)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      raw <- readFile path
      -- We need to read the input strictly to avoid accidentally mixing lazy IO
      -- and nondeterminism (which would lead to bogus results and end-of-stream
      -- errors).
      let input = (read <$>) . words <$> (lines $!! raw)
          part1 = result input
          part2 = result $ reverse <$> input
      putStrLn $ "Part 1: " ++ show part1
      putStrLn $ "Part 2: " ++ show part2
    _ -> putStrLn "Usage: day09 <input>"
