-- CIS194 Introduction to Haskell: Homework 1
-- Daniel Cookman


-- Ex 1
toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)
-- toDigits n
--  |  n <= 0    = []
--  |  otherwise = (toDigits(n `div` 10)) ++ [n `mod` 10]


toDigitsRev :: Integer -> [Integer]
-- toDigitsRev n = reverse (toDigits n)
toDigitsRev n
 |  n <= 0    = []
 |  otherwise = (n `mod` 10):(toDigitsRev(n `div` 10))


-- Ex 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []       = []
doubleEveryOther [x]      = [x]
doubleEveryOther (x:xs)
  | odd  (length (x:xs))  = x:doubleEveryOther(xs)
  | even (length (x:xs))  = (2*x):doubleEveryOther(xs)


-- Ex 3
sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs) = sum(toDigits x) + sumDigits xs

-- Ex 4
validate :: Integer -> Bool
validate n = sumDigits(doubleEveryOther(toDigits n)) `mod` 10 == 0

-------------------------------

-- Ex 5
type Peg = String
type Move = (Peg, Peg)


hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n <= 0     = []
  | n == 1     = [(a, b)]
  | otherwise  = (hanoi (n-1) a c b) ++ (hanoi 1 a b c) ++ (hanoi (n-1) c b a)


-- Ex 6
hanoiFour :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoiFour n a b c d
  | n <= 2    = hanoi n a b c
  | otherwise = 