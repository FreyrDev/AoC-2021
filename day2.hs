import Data.Char ( digitToInt )

parse :: String -> [(Char,Int)]
parse input = map (\x -> (head x, digitToInt (last x))) (lines input)

day2_1 :: [(Char,Int)] -> Int
day2_1 input = uncurry (*) $ go (0,0) input
    where go (x,y) [] = (x,y)
          go (x,y) (z:zs) | fst z == 'f' = go (x + snd z, y) zs
                          | fst z == 'u' = go (x, y - snd z) zs
                          | fst z == 'd' = go (x, y + snd z) zs
                          | otherwise    = (0,0)

main :: IO ()
main = do
    input <- readFile "inputs/day2.txt"
    print . day2_1 . parse $ input
