day8_1 :: String -> Int
day8_1 = length . filter (`elem` [2,3,4,7]) . map length . outputs
    where outputs = concatMap (words . drop 2 . dropWhile (/= '|')) . lines

main :: IO ()
main = do
    print . day8_1 =<< readFile "inputs/day8.txt"
