{-# OPTIONS_GHC -Wall #-}
module Golf where
-- import Data.List
-- import Safe

-- Ex 1

skips :: [a] -> [[a]]
skips [] = []
skips x  = map (\n -> map last (split x n)) [1..length x]

split :: [a] -> Int -> [[a]]
split [] _ = []
split xs n = (take n xs):(split(drop n xs) n)


-- Ex 2

