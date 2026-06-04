module BF where
-- This is the parsed program, thus all loops must
-- be matched correctly
data Instr =
  NextData
  | PrevData
  | Inc
  | Dec
  | Output
  | Input
  | Loop [Instr]
  deriving Show

parseSingle :: Char -> Maybe Instr
parseSingle = f
  where
    f '>' = Just NextData
    f '<' = Just PrevData
    f '+' = Just Inc
    f '-' = Just Dec
    f '.' = Just Output
    f ',' = Just Input
    f _ = Nothing

parseClose :: String -> Maybe ([Instr], String)
parseClose [] = Nothing
parseClose (']' : rest) = Just ([], rest)
parseClose ('[' : rest) = do
  (is, rest') <- parseClose rest
  (is', rest'') <- parseClose rest'
  return (Loop is : is', rest'')
parseClose (c : rest) = do
  i <- parseSingle c
  (is, rest') <- parseClose rest
  return (i : is, rest')

parse :: String -> Maybe [Instr]
parse [] = Just []
parse ('[' : rest) = do
  (is, s) <- parseClose rest
  is' <- parse s
  return (Loop is : is')
parse (c : rest) = do
  i <- parseSingle c
  is <- parse rest
  return (i : is)

