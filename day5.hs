{-# LANGUAGE TupleSections #-}
import Data.List (group, sort)

parse :: String -> [((Int,Int),(Int,Int))]
parse input = map ((\xs -> (toTuple (head xs), toTuple (last xs))) . words) $ lines input
    where toTuple x = read ("(" ++ x ++ ")")

(...) :: (Ord a, Enum a) => a -> a -> [a]
a ... b
  | a <= b = [a .. b]
  | otherwise = [a, pred a .. b]
infixl 7 ...

countCoords :: (Int -> Bool) -> [(Int,Int)] -> Int
countCoords cond = length . filter cond . map length . group . sort

day5_1 :: [((Int,Int),(Int,Int))] -> Int
day5_1 = countCoords (>1) . concatMap (\((x1,y1),(x2,y2)) -> coords x1 y1 x2 y2) . filter orthogonal
    where coords x1 y1 x2 y2 | x1 == x2  = (x1,) <$> y1...y2
                             | y1 == y2  = (,y1) <$> x1...x2
                             | otherwise = []
          orthogonal ((x1,y1),(x2,y2)) = x1 == x2 || y1 == y2

day5_2 :: [((Int,Int),(Int,Int))] -> Int
day5_2 = countCoords (>1) . concatMap (\((x1,y1),(x2,y2)) -> coords x1 y1 x2 y2) . filter straight
   where coords x1 y1 x2 y2
           | y1 == y2  = (,y1) <$> x1 ... x2
           | x1 == x2  = (x1,) <$> y1 ... y2
           | otherwise = betweenDiagonal x1 y1 x2 y2
         betweenDiagonal x1 y1 x2 y2
           | x2 > x1 && y2 > y1 = (\x -> (x, x + (y1-x1))) <$> x1...x2
           | x1 > x2 && y2 > y1 = (\x -> (x, (y2+x2) - x)) <$> x1...x2
           | x1 > x2 && y1 > y2 = (\x -> (x, x + (y1-x1))) <$> x1...x2
           | x2 > x1 && y1 > y2 = (\x -> (x ,(y2+x2) - x)) <$> x1...x2
           | otherwise = [(x1,y1)]
         straight ((x1,y1),(x2,y2)) = x1 == x2 || y1 == y2 || (abs (x2 - x1) == abs (y2 - y1))

main :: IO ()
main = do
    input <- readFile "inputs/day5.txt"
    print . day5_1 . parse $ input
    print . day5_2 . parse $ input
