-- import Data.List (transpose)
-- --parse :: String -> ([Int],[[Int]])

-- draws :: String -> [Int]
-- draws input = read ("[" ++ head (lines input) ++ "]")

-- boards :: String -> [[[Int]]]
-- boards = chunk 5 . map (map read . words) . filter (not.null) . tail . lines
--     where chunk n xs | n <= 0 || null xs = []
--                      | otherwise = take n xs : chunk n (drop n xs)

-- day4_1 :: String -> [[Int]]
-- day4_1 input = draw (draws input) wins base
--     where draw (x:xs) (y:ys) acc = draw xs ys (map (map (if y == x then const 1 else const 0)) ys)
--           draw _ _ ys = ys
--           wins = concat (boards input ++ map transpose (boards input))
--           base = map (map (const 0)) wins

-- main :: IO ()
-- main = do
--     input <- readFile "inputs/day4.txt"
--     print . day4_1 $ input




-- import Data.Char (digitToInt)

-- parse :: String -> [[Int]]
-- parse = map (map digitToInt).  lines

-- day11_1 :: [[Int]] -> [[Int]]
-- day11_1 = step
--     where step = map (map (\x if +1))

-- main :: IO ()
-- main = do
--     input <- readFile "inputs/day11.txt"
--     print . day11_1 . parse $ input
