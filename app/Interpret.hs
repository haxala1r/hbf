module Interpret where
import Data.Word
import Data.Char
import Control.Monad
import BF

-- This is the memory, the tape.
-- we represent it as a zipper in memory.
-- Left list, current cell, right list.
-- Note that this structure makes all operations
-- O(1) despite being functionally pure and lazy.
--
-- Note: We use a custom InfiniteList type here.
-- this allows the compiler to verify that the
-- right tape will never be empty, thus we
-- don't need to check for it in executeOne.
data InfiniteList a = Cons a (InfiniteList a)
data Tape = Tape [Word8] Word8 (InfiniteList Word8)

-- empty tape has an infinite number of zero cells
-- to the right.
-- note that the zeroes will only be materialized on demand.
infRepeat :: a -> InfiniteList a
infRepeat x = Cons x (infRepeat x)
newTape :: Tape
newTape = Tape [] 0 (infRepeat 0)


executeOne :: Instr -> Tape -> IO Tape

executeOne NextData (Tape ls c (Cons r rs)) =
  return $ Tape (c : ls) r rs
executeOne PrevData t = f t
  where
    f (Tape [] c rs) = return $ Tape [] c rs
    f (Tape (l : ls) c rs) = return $ Tape ls l (Cons c rs)
executeOne Inc (Tape ls c rs) = return $ Tape ls (c + 1) rs
executeOne Dec (Tape ls c rs) = return $ Tape ls (c - 1) rs
executeOne Output (Tape ls c rs) = do
  putChar $ chr $ fromEnum c
  return $ Tape ls c rs
executeOne Input (Tape ls _ rs) = do
  c <- getChar
  return $ Tape ls (toEnum $ ord c) rs

-- just like when parsing, the loops are the only
-- tricky part, but they're not hard
executeOne (Loop is) (Tape ls c rs) =
  if c == 0 then
    return $ Tape ls c rs -- if zero, just skip
  else
    -- if non-zero, execute the entire body, then
    -- keep executing forever until you see zero.
    do
      t <- executeAll is (Tape ls c rs)
      executeOne (Loop is) t

executeAll :: [Instr] -> Tape -> IO Tape
executeAll is t =
  foldM (\t' i -> executeOne i t') t is  


-- test
helloWorld :: [Instr]
helloWorld = case parse "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++." of
  Nothing -> error "cant parse"
  Just i -> i
