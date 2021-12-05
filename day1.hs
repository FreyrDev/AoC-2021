stringToInts :: String -> [Int]
stringToInts input = map read (words input)

day1_1 :: [Int] -> Int
day1_1 []     = 0
day1_1 (x:xs) = length $ filter (<0) $ zipWith (-) (x:xs) xs

day1_2 :: [Int] -> Int
day1_2 []       = 0
day1_2 [_]      = 0
day1_2 (x:y:xs) = day1_1 $ zipWith3 (\x y z -> x + y + z) (x:y:xs) (y:xs) xs

main :: IO ()
main = do
    input <- readFile "inputs/day1.txt"
    print . day1_1 . stringToInts $ input
    print . day1_2 . stringToInts $ input
