import Data.List (group, sort)

parse :: String -> [((Int,Int),(Int,Int))]
parse input = map ((\xs -> (toTuple (head xs), toTuple (last xs))) . words) $ lines input
    where toTuple x = read ("(" ++ x ++ ")")

day5_1 :: [((Int,Int),(Int,Int))] -> Int
day5_1 = length . filter (>1) . map length . group . sort . concatMap (\((x1,y1),(x2,y2)) -> coords x1 y1 x2 y2) . filter orthogonal
    where coords x1 y1 x2 y2 = if x1 == x2 then betweenY y1 y2 x1 else betweenX x1 x2 y1
          betweenY p1 p2 other = [(other,y) | y <- [min p1 p2..max p1 p2]]
          betweenX p1 p2 other = [(x,other) | x <- [min p1 p2..max p1 p2]]
          orthogonal ((x1,y1),(x2,y2)) = x1 == x2 || y1 == y2

day5_2 :: [((Int,Int),(Int,Int))] -> Int
day5_2 = length . filter (>1) . map length . group . sort . concatMap (\((x1,y1),(x2,y2)) -> coords x1 y1 x2 y2) . filter straight
   where coords x1 y1 x2 y2
           | abs (x2 - x1) == abs (y2 - y1) = betweenDiagonal x1 y1 x2 y2
           | x1 == x2               = betweenY y1 y2 x1
           | otherwise               = betweenX x1 x2 y1
         betweenY p1 p2 other = [(other,y) | y <- [min p1 p2..max p1 p2]]
         betweenX p1 p2 other = [(x,other) | x <- [min p1 p2..max p1 p2]]
         betweenDiagonal x1 y1 x2 y2
           | x2 > x1 && y2 > y1 = [(x,x+(y1-x1)) | x <- [x1..x2]]
           | x1 > x2 && y2 > y1 = [(x,(y2+x2)-x) | x <- [x2..x1]]
           | x1 > x2 && y1 > y2 = [(x,x+(y1-x1)) | x <- [x2..x1]]
           | x2 > x1 && y1 > y2 = [(x,(y2+x2)-x) | x <- [x1..x2]]
           | otherwise = [(x1,y1)]
         straight ((x1,y1),(x2,y2)) = x1 == x2 || y1 == y2 || (abs (x2 - x1) == abs (y2 - y1))

main :: IO ()
main = do
    input <- readFile "inputs/day5.txt"
    print . day5_1 . parse $ input
    print . day5_2 . parse $ input
