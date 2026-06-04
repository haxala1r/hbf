{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import BF
import Interpret
import System.IO
import CBackend
import Options.Applicative


exec :: (Char -> IO ()) -> [Instr] -> IO ()
exec f is = do
  _ <- executeAll f is newTape
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


data CmdOpts = CmdOpts { compile :: Bool, outFile :: String }

parser :: Parser CmdOpts
parser = CmdOpts
  <$> switch (long "compiler"
             <> short 'c'
             <> help "whether to compile to C code instead of interpreting")
  <*> strOption (long "output"
                <> short 'o'
                <> value ""
                <> metavar "OUT"
                <> help "write output to OUT instead of stdout")

doMain :: CmdOpts -> IO ()
doMain opts = do
  input <- getAll
  case (parse input, outFile opts) of
    (Just is, "") ->
      if (compile opts) then
        print $ emitC is
      else
        exec putChar is
    (Just is, f) ->
      withFile f WriteMode
      (\handle -> do
          if (compile opts) then
            hPutStr handle $ show $ emitC is
          else
            exec (hPutChar handle) is)
    (Nothing, _) ->
      putStrLn "parse error (possibly an unmatched ])"


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  doMain =<< execParser opts
    where
      opts = info (parser <**> helper)
        (fullDesc
        <> progDesc "Interpret Brainfuck, or compile BF to C"
        <> header "Interpreter/Compiler for Brainfuck")
