module Main (main) where
import Interpret
import System.IO

exec :: String -> IO ()
exec s = case parse s of
  Nothing -> putStrLn "Cannot parse! probably a mismatched ]"
  Just is -> (do
    _ <- executeAll is newTape
    return ())


getAll :: IO String
getAll = aux []
  where
    aux acc = do
      done <- isEOF
      if done
      then return acc
      else do
        l <- getLine
        aux (acc ++ l)
        
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  input <- getAll
  exec input
