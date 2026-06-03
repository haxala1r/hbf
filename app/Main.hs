module Main (main) where
import Interpret


exec :: String -> IO ()
exec s = case parse s of
  Nothing -> putStrLn "Cannot parse! probably a mismatched ]"
  Just is -> (do
    _ <- executeAll is newTape
    return ()         )
main :: IO ()
main = do
  input <- getLine
  exec input
