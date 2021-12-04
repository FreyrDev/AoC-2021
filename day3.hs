import Data.Char (digitToInt)
import Data.List (partition, transpose)

parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

gamma, epsilon :: [[Int]] -> [Int]
gamma = map ((\x -> if length (fst x) > length (snd x) then 0 else 1) .  partition (==0)) . transpose
epsilon = map ((`mod` 2) . (+1)) . gamma

toDecimal :: [Int] -> Int
toDecimal = foldl ((+) . (*2)) 0

day3_1 :: [[Int]] -> Int
day3_1 input = toDecimal (gamma input) * toDecimal (epsilon input)

day3_2 :: [[Int]] -> Int
day3_2 input = toDecimal (oxy input) * toDecimal (co2 input)
    where oxy = rating 0 gamma
          co2 = rating 0 epsilon
          rating _ _ [x] = x
          rating i f xs  = rating (i+1) f (filter ((f xs !! i ==) . (!! i)) xs)

main :: IO ()
main = do
    input <- readFile "inputs/day3.txt"
    print . day3_1 . parse $ input
    print . day3_2 . parse $ input
