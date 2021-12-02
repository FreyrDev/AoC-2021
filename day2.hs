import Data.Char ( digitToInt )

parse :: String -> [(Char,Int)]
parse input = map (\x -> (head x, digitToInt (last x))) (lines input)

day2_1 :: [(Char,Int)] -> Int
day2_1 = uncurry (*) . go (0,0)
    where go (x,y) [] = (x,y)
          go (x,y) (z:zs) | fst z == 'f' = go (x + snd z, y) zs
                          | fst z == 'u' = go (x, y - snd z) zs
                          | fst z == 'd' = go (x, y + snd z) zs
                          | otherwise    = (0,0)

day2_2 :: [(Char,Int)] -> Int
day2_2 = uncurry (*) . go 0 (0,0)
    where go a (x,y) [] = (x,y)
          go a (x,y) (z:zs) | fst z == 'f' = go a (x + snd z, y + (a * snd z)) zs
                            | fst z == 'u' = go (a - snd z) (x,y) zs
                            | fst z == 'd' = go (a + snd z) (x,y) zs
                            | otherwise    = (0,0)

main :: IO ()
main = do
    input <- readFile "inputs/day2.txt"
    print . day2_2 . parse $ input
