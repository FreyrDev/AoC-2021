parse :: String -> [Int]
parse input = read ("[" ++ input ++ "]")

sumN :: Int -> Int
sumN n = n * (n+1) `div` 2

day7 :: (Int -> Int) -> [Int] -> Int
day7 f input = minimum $ zipWith fuelUsage lists allSorted
    where fuelUsage xs x = sum (map (f . abs . (x-)) xs)
          lists = replicate (length input) input
          allSorted = [minimum input .. maximum input]

main :: IO ()
main = do
    input <- readFile "inputs/day7.txt"
    print . day7 id . parse $ input
    print . day7 sumN . parse $ input
