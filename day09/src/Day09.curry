import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      raw <- readFile path
      let input = (read <$>) . words <$> lines raw
      print (input :: [[Int]])
    _ -> putStrLn "Usage: day09 <input>"
