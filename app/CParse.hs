module CParse where

import Control.Applicative

-- ^
-- In this module, we parse (a subset of) C and compile
-- it into BrainFuck code.
--
-- First, we must parse the subset of C code that we're
-- going to compile into brainfuck.
--
-- Then, we will perform some analysis (such as typechecking).
--
-- Then we will allocate memory for C variables, calculate their addresses, and then emit the final brainfuck code.

-- |
-- In the first stage, we parse the code into a very
-- simple, dumb tree. In the second stage we will perform
-- type checking.
data CProgram = CProgram [CVarDecl] [CFnDecl]
  deriving (Show)

data CType = CType String
  deriving (Show)

{-
Variable declaration/definition of the form:
-}
data CVarDecl = CVarDecl CType String (Maybe CExpr)
  deriving (Show)

data CFnDecl = CFnDecl CType String [(CType, String)] [CVarDecl] [CStmt]
  deriving (Show)

data CExpr
  = Symbol String
  | IntLiteral Int
  | StringLiteral String
  | FunCall CExpr [CExpr]
  | Add CExpr CExpr
  | Sub CExpr CExpr
  | Multiply CExpr CExpr
  deriving (Show)

-- |
-- May only appear in the body of a function, NOT the top-level.
-- Also in for, while, if bodies
data CStmt
  = ExprStmt CExpr
  | Assign String CExpr
  | WhileLoop CExpr [CStmt]
  deriving (Show)

-- |
-- We use this abstract type to parse a string as C code.
-- It is an Applicative Functor (also implementing Alternative).
-- This allows us to combine parsers in a very convenient manner,
-- so we can write our parser from very simple building blocks.
--
-- Of course, the downside to this setup would be that we can't
-- really parse the entire C standard correctly - C syntax is
-- notoriously context dependent, so a fully standards-compliant
-- C compiler would be out of scope for this project.
--
-- That is, of course, leaving aside the matter that brainfuck is
-- not an optimal backend for C.
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

{- |
Technically the Functor instance here is redundant:
if we were to define Applicative in a manner that did
not rely on <$> then the Functor instance would be
trivial: `fmap = liftA`

I don't do this here because I wanted to implement
all typeclasses individually here.
-}
instance Functor Parser where
  fmap f p = Parser inner
    where
      inner s = case runParser p s of
        Nothing -> Nothing
        Just (a, rest) -> Just (f a, rest)

instance Applicative Parser where
  pure x = Parser $ \s -> Just (x, s)
  pf <*> p = Parser $ \s ->
    case runParser pf s of
      Nothing -> Nothing
      Just (f, rest) ->
        runParser (f <$> p) rest

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  p1 <|> p2 = Parser $ \s ->
    case runParser p1 s of
      Just x -> Just x
      Nothing -> runParser p2 s

done :: Parser ()
done = Parser $ \s -> Just ((), s)

digit :: Parser Int
digit = Parser $ f
  where
    f ('0' : rest) = Just (0, rest)
    f ('1' : rest) = Just (1, rest)
    f ('2' : rest) = Just (2, rest)
    f ('3' : rest) = Just (3, rest)
    f ('4' : rest) = Just (4, rest)
    f ('5' : rest) = Just (5, rest)
    f ('6' : rest) = Just (6, rest)
    f ('7' : rest) = Just (7, rest)
    f ('8' : rest) = Just (8, rest)
    f ('9' : rest) = Just (9, rest)
    f _ = Nothing

digits :: Parser [Int]
digits = some digit

intLiteral :: Parser Int
intLiteral = Parser $ \s ->
  case runParser digits s of
    Nothing -> Nothing
    Just (is, s') ->
      Just (foldl (\x y -> x * 10 + y) 0 is, s')

-- Parse a single character only
mkSingle :: Char -> Parser Char
mkSingle c = Parser f
  where
    f (c' : rest)
      | c == c' = Just (c, rest)
      | otherwise = Nothing
    f [] = Nothing

mkValidCharParser :: [Char] -> Parser Char
mkValidCharParser cs = Parser f
  where
    f (c' : rest)
      | elem c' cs = Just (c', rest)
      | otherwise = Nothing
    f [] = Nothing

singleQuote :: Parser Char
singleQuote = mkSingle '\''

doubleQuote :: Parser Char
doubleQuote = mkSingle '"'

anySingleChar :: Parser Char
anySingleChar = Parser f
  where
    f (c : rest) = Just (c, rest)
    f [] = Nothing

whitespace :: Parser ()
whitespace = (many $ mkValidCharParser " \t\n\r") *> pure ()

charLiteral :: Parser Char
charLiteral = singleQuote *> anySingleChar <* singleQuote

stringLiteral :: Parser String
stringLiteral = doubleQuote *> (some anySingleChar) <* doubleQuote

symbol :: Parser String
symbol = whitespace *> some (mkValidCharParser "abcdefghijklmnopqrstuwxvyzABCDEFGHIJKLMNOPQRSTUWXVYZ0123456789_")

symbolExpr :: Parser CExpr
symbolExpr = Parser $ \s -> do
  (n, s') <- runParser symbol s
  pure (Symbol n, s')

intExpr :: Parser CExpr
intExpr = IntLiteral <$> (whitespace *> intLiteral)

cadd :: Parser CExpr
cadd =
  ( Add
      <$> cmul
      <* whitespace
      <* mkSingle '+'
      <*> cmul
  )
    <|> ( Sub
            <$> cmul
            <* whitespace
            <* mkSingle '-'
            <*> cmul
        )
    <|> cmul

cmul :: Parser CExpr
cmul =
  ( Multiply
      <$> cfactor
      <* whitespace
      <* mkSingle '*'
      <*> cfactor
  )
    <|> cfactor

cfactor :: Parser CExpr
cfactor =
  intExpr
    <|> symbolExpr
    <|> (open *> cexpr <* close)
  where
    open = mkSingle '('
    close = mkSingle ')'

cexpr :: Parser CExpr
cexpr = whitespace *> cadd

cAssignRight :: Parser CExpr
cAssignRight = whitespace *> mkSingle '=' *> cexpr

ctype :: Parser CType
ctype = CType <$> symbol


-- Parses a variable declaration/definition.
varDecl :: Parser CVarDecl
varDecl = (CVarDecl <$> ctype <*> symbol <*> (Just <$> cAssignRight <|> pure Nothing))
  <* whitespace <* mkSingle ';'

-- parses the argument list part of a function
-- definition
funArgList :: Parser [(CType, String)]
funArgList = ((:) <$> one <*> (mkSingle ',' *> funArgList))
  <|> pure []
  where
    one = (,) <$> (CType <$> symbol) <*> symbol

-- Parses a function definition.
funDef :: Parser CFnDecl
funDef = CFnDecl <$> (CType <$> symbol)
  <*> symbol
  <*> (mkSingle '(' *> funArgList <* mkSingle ')')
  <*> (whitespace *> mkSingle '{' *> many varDecl)
  <*> (many stmt <* whitespace <*mkSingle '}')
exprStmt :: Parser CStmt
exprStmt = ExprStmt <$> cexpr

assignStmt :: Parser CStmt
assignStmt = Assign <$> symbol <*> cAssignRight

-- parses a single C statement
stmt :: Parser CStmt
stmt = (assignStmt <|> exprStmt) <* mkSingle ';'

cparser :: Parser CProgram
cparser = CProgram <$> many (varDecl <* whitespace) <*> many funDef

cparse :: String -> Maybe CProgram
cparse = (fmap fst) . runParser cparser
