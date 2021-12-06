import Data.List ( group, groupBy, sort, sortBy )
import Data.Bool ( bool )
import Data.Ord ( comparing )
import Data.Function ( on )

parse :: String -> [Integer]
parse input = read ("[" ++ input ++ "]")

merge :: [(Integer,Integer)] -> [(Integer,Integer)]
merge = map (foldl1 (\(x,y) (_,b) -> (x,y+b))) . groupBy (on (==) fst) . sortBy (comparing fst)

day6 :: Integer -> [Integer] -> Integer
day6 days input = go 0 (count input)
    where go n xs
            | n == days = foldl ((. snd) . (+)) 0 xs
            | otherwise = go (succ n) (merge . concatMap (create . shift) $ xs)
          create (num,freq) = bool [(num,freq)] [(6,freq),(8,freq)] (num == (-1))
          shift (num,freq) = (pred num,freq)
          count = map (\x -> (head x, toInteger (length x))) . group . sort
          
day6V2 :: Integer -> [Integer] -> Integer
day6V2 days input = go 0 (map (count input) [0,1,2,3,4,5,6,7,8])
    where go n xs
            | n == days = sum xs
            | otherwise = go (succ n) (shift xs)
          shift (x:xs) = take 6 xs ++ [xs !! 6 + x] ++ [xs !! 7] ++ [x]
          count xs n = toInteger $ length (filter (==n) xs)

main :: IO ()
main = do
    input <- readFile "inputs/day6.txt"
    print . day6 80 . parse $ input
    print . day6 256 . parse $ input
