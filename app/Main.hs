module Main (main) where
import Interpret
import System.IO

exec :: String -> IO ()
exec s = case parse s of
  Nothing -> putStrLn "Cannot parse! probably a mismatched ]"
  Just is -> (do
    _ <- executeAll is newTape
    return ()         )
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  input <- getLine
  exec input
