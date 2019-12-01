module Day1 where

fuel :: Integer -> Integer
fuel = subtract 2 . flip quot 3

allFuel :: Integer -> Integer
allFuel = sum . takeWhile (>0) . tail . iterate fuel

main :: IO ()
main = do
  masses <- fmap (fmap read . words) $ readFile "inputs/1.txt"
  let part1 = sum . fmap fuel $ masses
  let part2 = sum . fmap allFuel $ masses
  putStrLn $ formatResult part1 part2
  where
    formatResult r1 r2 =
      unlines [ "Part1: " <> show r1
              , "Part2: " <> show r2
              ]
