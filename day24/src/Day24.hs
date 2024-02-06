import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (readFile')

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
