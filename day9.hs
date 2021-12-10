import Data.Char (digitToInt)
import Data.List (transpose)

parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

day9 :: [[Int]] -> Int
day9 input = sum $ zipWith riskLevel (concat . transpose . lowPoint . transpose $ input) (concat . lowPoint $ input)
    where lowPoint = map (\xs -> zipWith3 (\x y z -> if y<x && y<z then y else 10) (10:xs) xs (tail xs ++ [10]))
          riskLevel x y = if x<10 && y<10 then x+1 else 0

main :: IO ()
main = do
    print . day9 . parse =<< readFile "inputs/day9.txt"
