module CBackend where
import BF

{-
The C AST presented here is very very simple,
but we really don't need anything more than this.
Strictly speaking we don't even really need a tree
structure, simple concatenation would work too -
but this allows for better pretty-printing of the
generated code.
-}

newtype CAst = CAst [CDef]

data CStmt =
  CStmt String
  | CIf String [CStmt]
  | CWhile String [CStmt]
data CDef =
  CInclude String
  | CGlobal String String
  | CFunc String String [CStmt]

-- we implement Show to pretty-print the C code
-- we still need helper functions to print with
-- correct indentation however (not required, but nice)
instance Show CDef where
  show (CInclude s) = "#include " ++ s ++ "\n"
  show (CGlobal t n) = t ++ " " ++ n ++  ";\n"
  show (CFunc t n body) =
    t ++ " " ++ n ++ "() {\n" ++ (concatMap (showStmt 1) body) ++ "}\n"
instance Show CAst where
  show (CAst is) = concatMap show is

nTabs :: Int -> String
nTabs i = take i $ repeat '\t'

indentOne :: Int -> String -> String
indentOne i s = (nTabs i) ++ s ++ "\n"

indent :: Int -> String -> String
indent i s = concatMap (indentOne i) (lines s)

showStmt :: Int -> CStmt -> String
showStmt i (CStmt s) = indent i (s ++ ";")
showStmt i (CIf cond body) =
  indent i ("if (" ++ cond ++ ") {\n" ++ (concatMap (showStmt (1)) body) ++ "}\n")
showStmt i (CWhile cond body) =
  indent i ("while (" ++ cond ++ ") {\n" ++ (concatMap (showStmt (1)) body) ++ "}\n")

-- convert from BF instr to C code
emitOne :: Instr -> [CStmt]
emitOne NextData =
  [CIf "curr == right"
    (map CStmt [
        "left = realloc(left, size * 2)"
        ,"size = size * 2"
        ,"right = left + size"
        ])
  ,CStmt "curr++"
  ]
emitOne PrevData =
  [CIf "curr != left"
    [CStmt "curr--"]]
emitOne Inc = [CStmt "(*curr)++"]
emitOne Dec = [CStmt "(*curr)--"]
emitOne Output = [CStmt "putchar((char)*curr)"
                 ,CStmt "fflush(stdout)"]
emitOne Input = [CStmt "buf = getchar()"
                ,CStmt "*curr = (buf < 0) ? *curr : buf"]
emitOne (Loop is) =
  [CWhile "*curr != 0" (concatMap emitOne is)]

emitAll :: [Instr] -> [CStmt]
emitAll = concatMap emitOne

-- We compile the entire brainfuck program
-- directly into main
emitC :: [Instr] -> CAst
emitC is = CAst [CInclude "<stdio.h>"
                ,CInclude "<stdlib.h>"
                ,CInclude "<stdint.h>"
                ,CInclude "<string.h>"
                ,CGlobal "uint8_t*" "left"
                ,CGlobal "uint8_t*" "curr"
                ,CGlobal "uint8_t*" "right"
                ,CGlobal "uint64_t" "size"
                ,CGlobal "int" "buf"
                ,CFunc "int" "main" body]
  where
    body = [CStmt "size = 300000"
           ,CStmt "left = malloc(size)"
           ,CStmt "memset(left, 0, size)"
           ,CStmt "curr = left"
           ,CStmt "right = left + size"] ++ emitAll is ++
           [CStmt "return 0"]

writeTo :: String -> String -> IO ()
writeTo path code =
  case (parse code) of
    Just x -> writeFile path $ show $ emitC x
    Nothing -> writeFile path "parse failure"
