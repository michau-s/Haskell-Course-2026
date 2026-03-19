{-# LANGUAGE BangPatterns #-}
module Solution where

-- EXERCISE 3
sieve :: [Int] -> [Int]
sieve [] = []
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

primesTo :: Int -> [Int]
primesTo n = sieve [2..n]

isPrime :: Int -> Bool
isPrime n
  | n < 2     = False
  | otherwise = n `elem` primesTo n

-- EXERCISE 1
goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n = 
  [ (p, q) 
  | p <- [2 .. n `div` 2]
  , let q = n - p
  , isPrime p
  , isPrime q
  ]

-- EXERCISE 2
coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs xs = 
  [ (x, y) 
  | x <- xs
  , y <- xs
  , x < y
  , gcd x y == 1 
  ]


-- EXERCISE 4
matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul [] _ = []
matMul _ [] = []
matMul a@(rowA:_) b@(rowB:_) =
  [ [ sum [ (a !! i !! k) * (b !! k !! j) | k <- [0 .. p - 1] ]
    | j <- [0 .. n - 1] ]
  | i <- [0 .. m - 1] ]
  where
    m = length a
    p = length rowA
    n = length rowB

-- EXERCISE 5
permutations :: Eq a => Int -> [a] -> [[a]]
permutations 0 _  = [[]]
permutations k xs = 
  [ y:zs 
  | y <- xs
  , zs <- permutations (k - 1) (remove y xs) 
  ]

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove x (y:ys)
  | x == y    = ys
  | otherwise = y : remove x ys

-- EXERCISE 6
-- (a)
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y     = x : merge xs (y:ys)
  | x > y     = y : merge (x:xs) ys
  | otherwise = x : merge xs ys

-- (b)
hamming :: [Integer]
hamming = 1 : merge (map (*2) hamming) 
                    (merge (map (*3) hamming) 
                           (map (*5) hamming))

-- EXERCISE 7
power :: Int -> Int -> Int
power b e = powerAcc b e 1
  where
    powerAcc :: Int -> Int -> Int -> Int
    powerAcc _ 0 !acc = acc
    powerAcc base exp !acc = powerAcc base (exp - 1) (acc * base)


-- EXERCISE 8
-- Version 1: Using seq
listMaxSeq :: [Int] -> Int
listMaxSeq (x:xs) = go x xs
  where
    go acc [] = acc
    go acc (y:ys) = let nextAcc = max acc y
                    in nextAcc `seq` go nextAcc ys

-- Version 2: Using Bang Patterns
listMaxBang :: [Int] -> Int
listMaxBang (x:xs) = go x xs
  where
    go !acc [] = acc
    go !acc (y:ys) = go (max acc y) ys


-- EXERCISE 9
-- (a)
primes :: [Int]
primes = sieve [2..]

-- (b)
isPrimeInf :: Int -> Bool
isPrimeInf n
  | n < 2     = False
  | otherwise = case dropWhile (< n) primes of
                  (x:_) -> n == x
                  []    -> False

-- EXERCISE 10
-- (a)
meanLazy :: [Double] -> Double
meanLazy xs = go 0 0 xs
  where
    go sum len [] = sum / len
    go sum len (y:ys) = go (sum + y) (len + 1) ys

-- (b)
meanStrict :: [Double] -> Double
meanStrict xs = go 0 0 xs
  where
    go !sum !len [] = sum / len
    go !sum !len (y:ys) = go (sum + y) (len + 1) ys

-- (c)
meanVar :: [Double] -> (Double, Double)
meanVar xs = go 0 0 0 xs
  where
    go !sum !sumSq !len [] = 
      let mu = sum / len
          var = (sumSq / len) - (mu * mu)
      in (mu, var)
    go !sum !sumSq !len (y:ys) = 
      go (sum + y) (sumSq + y * y) (len + 1) ys