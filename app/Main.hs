{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import BF
import Interpret
import System.IO
import CBackend


exec :: [Instr] -> IO ()
exec is = do
  _ <- executeAll is newTape
  return ()

cleanStr :: String -> String
cleanStr = filter (\x -> x /= ' ' && x /= '\t' && x /= '\n')

getAll :: IO String
getAll = aux []
  where
    aux acc = do
      done <- isEOF
      if done
      then return $ cleanStr acc
      else do
        l <- getLine
        aux (acc ++ l)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  input <- getAll
  case parse input of
    Just is -> 
      print $ emitC is
    Nothing ->
      putStrLn "parse error (possibly an unmatched ])"
