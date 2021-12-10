import Data.List (sort)

parse :: String -> [String]
parse = lines

starts, finals :: String
starts = "([{<"
finals = ")]}>"

finalOf :: Char -> Char
finalOf x = case x of
    '(' -> ')'
    '[' -> ']'
    '{' -> '}'
    '<' -> '>'
    _   -> ' '

valueOf :: Char -> Int
valueOf x = case x of
    ')' -> 3
    ']' -> 57
    '}' -> 1197
    '>' -> 25137
    _   -> 0

day10_1 :: [String] -> Int
day10_1 = sum . map (valueOf . go "")
    where go _ "" = ' '
          go acc (x:xs)
            | x `elem` starts = go (acc++[x]) xs
            | x `elem` finals = if x == finalOf (last acc) then go (init acc) xs else x
            | otherwise = ' '

day10_2 :: [String] -> Int
day10_2 = middle . sort . filter (/=0) . map (scoreFold 0 . reverse . map finalOf . go "")
    where go acc "" = acc
          go acc (x:xs)
            | x `elem` starts = go (acc++[x]) xs
            | x `elem` finals = if x == finalOf (last acc) then go (init acc) xs else ""
            |otherwise = ""
          scoreFold acc "" = acc
          scoreFold acc (x:xs)
            | x == ')' = scoreFold (5*acc + 1) xs
            | x == ']' = scoreFold (5*acc + 2) xs
            | x == '}' = scoreFold (5*acc + 3) xs
            | x == '>' = scoreFold (5*acc + 4) xs
            | otherwise = 0
          middle xs = (!! (length xs `div` 2)) xs

main :: IO ()
main = do
    input <- readFile "inputs/day10.txt"
    print . day10_1 . parse $ input
    print . day10_2 . parse $ input
