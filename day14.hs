import Control.Arrow ((&&&))
import Control.Monad (liftM2)
import Data.List (sort,group)

parse :: String -> (String, [(String, Char)])
parse = (\(x:xs) -> (x,map (take 2 &&& last) xs)) . lines

day14 :: (String, [(String, Char)]) -> Int
day14 (template,rules) = liftM2 (-) maximum minimum . freqs $ iterate step template !! 10
    where freqs = sort . map length . group . sort
          step = (++ [last template]) . concatMap insert . pairs
          insert x = (head x :) . return . snd . head . filter ((== x) . fst) $ rules
          pairs = map (\(x,y) -> [x,y]) . (zip <*> tail)

main :: IO ()
main = do
    print . day14 . parse =<< readFile "inputs/day14.txt"
