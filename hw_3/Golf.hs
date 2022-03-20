{-# OPTIONS_GHC -Wall #-}
module Golf where

-- Ex 1
skips :: [a] -> [[a]]
{-|
    Top-level function which, given a list, produces a list of
    lists of all nth elements in the list, for all n from 1 to
    the size of the list.
-}
skips x  = map (map last . split x) [1..length x]

split :: [a] -> Int -> [[a]]
{-|
    Function which splits a given list into n equal chunks.
-}
split [] _ = []
split xs n = take n xs:split(drop n xs) n


-- Ex 2

localMaxima :: [Integer] -> [Integer]
{-|
    Function that returns a sublist containing only elements which are
    local minima: both the values immediately to the left and right
    are strictly less than this element. The first and last elements
    of the list are ignored.
-}
localMaxima x = map (!! 1) $ filter goodRegion $
                map (\n -> drop (n-1) $ take (n+2) x) [1..(length x -2)]

-- localRegions :: [Integer] -> [[Integer]]
-- localRegions x = map (\n -> drop (n-1) $ take (n+2) x) [1..(length x -2)]

goodRegion :: [Integer] -> Bool
{-|
    Helper function that returns whether both the left and right elements are
    strictly less than the central element of a length-3 list.
    Only a partial function, sorry.
-}
goodRegion []        = error "Must have exactly 3 elements in list"
goodRegion [_]       = error "Must have exactly 3 elements in list"
goodRegion [_,_]     = error "Must have exactly 3 elements in list"
goodRegion [a, b, c] = b > a && b > c
goodRegion (_:_)     = error "Must have exactly 3 elements in list"


-- Ex 3

histogram :: [Integer] -> String
{-|
    Function that, given a list of integers between 0-9 inclusive,
    outputs a vertical histogram showing how many of each number
    were in the input list.
-}
histogram x = makeHistStr ([length (filter (== n) x) | n<-[0..9]]) ++ "==========\n0123456789\n"

makeHistStr :: [Int] -> String
{-|
    Helper function which, given a list that denotes the number of
    each integer, recursively produces a string for the vertical
    histogram (excluding the x-axis).
-}
makeHistStr [0,0,0,0,0,0,0,0,0,0]   = ""
makeHistStr l = makeHistStr (map (\ x -> if x > 0 then x - 1 else 0) l) ++
    foldr (\ x -> (:) (if x > 0 then '*' else ' ')) "\n" l