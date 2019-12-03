module Day2 where

import           Data.Either (fromRight)
import qualified Data.Sequence as S
import           Data.Sequence ( Seq((:<|)), ViewL((:<)) )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T

type Program = S.Seq Integer

type Index = Int

data Instruction
  = Add
  | Mul
  | Halt

instruction :: Integer -> Instruction
instruction 1 = Add
instruction 2 = Mul
instruction 99 = Halt
instruction x = error ("Unexpected opcode: " <> show x)

execute :: Instruction -> Index -> Index -> Index -> Program -> Program
execute inst op1 op2 out p =
  case inst of
    Add  -> S.update out (v1 + v2) p
    Mul  -> S.update out (v1 * v2) p
    Halt -> p
  where
    v1 = S.index p op1
    v2 = S.index p op2
    
runProgram :: Index -> Program -> Program
runProgram current p =
  case inst of
    Halt -> p
    _    -> runProgram (current + 4) $ execute inst op1 op2 out p
  where
    inst = instruction (S.index p current)
    op1  = fromIntegral $ S.index p (current + 1)
    op2  = fromIntegral $ S.index p (current + 2)
    out  = fromIntegral $ S.index p (current + 3)

setup :: Integer -> Integer -> Program -> Program
setup noun verb p = p'
  where
    p' = S.update 2 verb (S.update 1 noun p)

finalState :: Integer -> Integer -> Program -> Integer
finalState n v p =
  let (result :< _) = S.viewl $ runProgram 0 $ setup n v p
  in result

findPair :: Program -> (Integer, Integer)
findPair p = head $ [(n,v) | n <- [0..99],
                             v <- [0..99],
                             finalState n v p == 19690720]

main :: IO ()
main = do
  s <- T.readFile "inputs/2.txt"
  let program = S.fromList $ fmap (fst . fromRight undefined . T.decimal) $ T.split (==',') s
  putStrLn $ "Part 1: " <> show (finalState 12 2 program)
  putStrLn $ "Part 2: " <> show ((\(x,y) -> 100 * x + y) $ findPair program)
