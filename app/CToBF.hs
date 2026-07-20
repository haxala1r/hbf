module CToBF where

import CParse
import Control.Arrow ((>>>))

data Instr =
  Inc
  | Dec
  | NextData
  | PrevData
  | Input
  | Output
  | BeginLoop
  | EndLoop

instance Show Instr where
  show Inc = "+"
  show Dec = "-"
  show NextData = ">"
  show PrevData = "<"
  show Input = ","
  show Output = "."
  show BeginLoop = "["
  show EndLoop = "]"

{-
In order to implement global variables,
we need to keep track of all such global variables, their names,
and their types.
Then we allocate space for each global variable (keeping track
of the address of each variable so that we can later resolve it).

We also need to emit the initialization code for each global
variable, so that it runs first (before main).
After that, we can emit code for the main function.

The rest of the tape after the global variables can be used as stack
space.
-}

-- keeps track of our tape layout
data Layout = Layout {
    program :: CProgram,
    vars :: [(String, Int)],
    end :: Int,
    initCode :: [CStmt]
  }
  deriving Show

mkLayout :: CProgram -> Layout
mkLayout (CProgram vs fs) = go vs (Layout {program = CProgram vs fs, vars = [], end = 0, initCode = []})
  where
    go [] l = l {vars = reverse $ vars l, initCode = reverse $ initCode l}
    go (v : vs') l = go vs' $ allocateVar v l

getSize :: CType -> Int
getSize CInt8 = 1 -- exactly one cell

addVarToInit :: String -> Maybe CExpr -> Layout -> Layout
addVarToInit _ Nothing l = l
addVarToInit n (Just e) l = l {initCode = (Assign n e) : initCode l}


allocateVar :: CVarDecl -> Layout -> Layout
allocateVar (CVarDecl t n v) l =
  addVarToInit n v $ l {
    vars = (n, end l) : vars l,
    end = end l + getSize t
  }

allocateScratch :: Int -> Layout -> (Int, Layout)
allocateScratch size l =
  (end l, l {
    end = end l + size
  })

findFn :: String -> CProgram -> CFnDecl
findFn name (CProgram _ fs) = go fs
  where
    go [] = error ("couldn't find function: " ++ name)
    go ((CFnDecl t n args vs body) : fs') =
      if n == name then CFnDecl t n args vs body
      else go fs'

findVar :: String -> Layout -> Maybe Int
findVar s l = f (vars l)
  where
    f [] = Nothing
    f ((n, i) : _) | n == s = Just i
    f ((_, _) : rest) | otherwise = f rest

{-
Keeps track of current code compilation
state.
Note: code is inserted into in reverse order.
the list will be reversed in the end,
and the final program will be obtained.

the cursor variable keeps track of the current
location of the cursor by our calculations. This
is quite important for the purposes of finding
our variables.
-} 
data Program = Program {
  layout :: Layout,
  cursor :: Int,
  code :: [Instr]
}
  deriving Show

mkProgram :: CProgram -> Program
mkProgram c = Program {layout = mkLayout c, cursor = 0, code = []}

addCode :: [Instr] -> Program -> Program
addCode is p =
  let c = code p in
  p {
    code = c ++ is
  }

addInstr :: Instr -> Program -> Program
addInstr i p = p {
  code = code p ++ [i]
}

getScratch1 :: Program -> (Int, Program)
getScratch1 p =
  let (i, l) = allocateScratch 1 (layout p) in
  (i, p {
    layout = l
  })

goLeft :: Int -> [Instr]
goLeft i = (take i $ repeat PrevData)

goRight :: Int -> [Instr]
goRight i = (take i $ repeat NextData)

goTo :: Int -> Int -> [Instr]
goTo from x = if from < x
  then goRight (x - from)
  else goLeft (from - x)

goTo_ :: Int -> Program -> Program
goTo_ x p = p {
  cursor = x,
  code = code p ++ (goTo (cursor p) x)
}

resetCurrent :: [Instr]
resetCurrent = [BeginLoop, Dec, EndLoop]

goAndReset :: Int -> Program -> Program
goAndReset target p =
  let p' = goTo_ target p in
  p' {
    code = code p' ++ resetCurrent
  }

compileExpr :: Int -> CExpr -> Program -> Program
compileExpr target (Symbol s) p =
  case findVar s (layout p) of
  Nothing -> error ("can't find symbol: " ++ s)
  Just s' ->
    let (scratch, p') = getScratch1 p in
    (goAndReset target >>>
    goAndReset scratch >>>
    goTo_ s' >>>
    addCode ([BeginLoop, Dec] ++ goTo s' scratch ++ [Inc] ++ goTo scratch target ++ [Inc] ++ goTo target s' ++ [EndLoop]) >>>
    goTo_ scratch >>>
    addCode ([BeginLoop, Dec] ++ goTo scratch s' ++ [Inc] ++ goTo s' scratch ++ [EndLoop]))
    p'

compileExpr target (IntLiteral i) p =
  let p' = goAndReset target p in
  addCode (take i $ repeat Inc) p'

compileExpr _ (StringLiteral _) _ = undefined
compileExpr _ (FunCall _ _) _ = undefined

{- -}
compileExpr target (Add e1 e2) p =
  let (scratch, p') = getScratch1 p in
  (compileExpr target e1 >>>
  compileExpr scratch e2 >>>
  goTo_ scratch >>>
  addCode ([BeginLoop, Dec] ++ goTo scratch target ++ [Inc] ++ goTo target scratch ++ [EndLoop]))
  p'

compileExpr target (Sub e1 e2) p =
  let (scratch, p') = getScratch1 p in
  (-- goAndReset target >>>
  -- goAndReset scratch >>>
  compileExpr target e1 >>>
  compileExpr scratch e2 >>>
  goTo_ scratch >>>
  addCode ([BeginLoop, Dec] ++ goTo scratch target ++ [Dec] ++ goTo target scratch ++ [EndLoop]))
  p'

compileExpr target (Multiply e1 e2) p =
  let (scratch1, p') = getScratch1 p in
  let (scratch2, p'') = getScratch1 p' in
  let (scratch3, p''') = getScratch1 p'' in
  (
    goAndReset target >>>
    goAndReset scratch3 >>>
    compileExpr scratch1 e1 >>>
    compileExpr scratch2 e2 >>>
    goTo_ scratch2 >>>
    addCode ([BeginLoop, Dec] ++ goTo scratch2 scratch1 ++
      ([BeginLoop, Dec] ++ goTo scratch1 target ++ [Inc]
        ++ goTo target scratch3 ++ [Inc] ++ goTo scratch3 scratch1 ++ [EndLoop])
      ++ goTo scratch1 scratch3 ++ -- copy result of e1 back.
      ([BeginLoop, Dec] ++ goTo scratch3 scratch1 ++ [Inc] ++ goTo scratch1 scratch3 ++ [EndLoop])
      ++ goTo scratch3 scratch2
      ++ [EndLoop])
  ) p'''

compileExpr target (Eq e1 e2) p = compileExpr target (Sub e1 e2) p
compileExpr _ (LessThan _ _) _ = undefined


compileStmt :: CStmt -> Program -> Program
compileStmt (ExprStmt e) p =
  let (scratch, p') = getScratch1 p in
  compileExpr scratch e p'

compileStmt (Assign s e) p =
  case findVar s (layout p) of
  Nothing -> error ("can't find symbol: " ++ s)
  Just i -> compileExpr i e p

compileStmt (WhileLoop cond body) p =
  let (scratch, p') = getScratch1 p in
  (goAndReset scratch >>>
  compileExpr scratch cond >>>
  addInstr BeginLoop >>>
  (\p'' -> foldl (\ a b -> compileStmt b a) p'' body) >>>
  compileExpr scratch cond >>>
  goTo_ scratch >>>
  addInstr EndLoop)
  p'


-- Wrapper stuff

compileC2BF :: CProgram -> Program
compileC2BF c =
  let p = mkProgram c in
  let i = initCode $ layout $ p in
  foldl' (\a b -> compileStmt b a) p i

