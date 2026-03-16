{-# LANGUAGE BangPatterns #-}

-- Exercise 3: Sieve of Eratosthenes (because it implements isPrime and this will be helpful in 1)

sieve :: [Int] -> [Int]
sieve [] = [] -- Base case: no numbers left, no primes left
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

primesTo :: Int -> [Int]
primesTo n = sieve [2..n]

isPrime :: Int -> Bool
isPrime n 
    | n < 2     = False
    | otherwise = n `elem` primesTo n

-- Exercise 1: Goldbach Pairs

goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n
    | n < 4 || odd n = [] -- Goldbach's conjecture applies to even integers greater than 2
    | otherwise = 
        [(p, q) 
        | p <- [2..n]
        , let q = n - p 
        , p <= q
        , isPrime p
        , isPrime q
        ]

-- Exercise 2:  Coprime Pairs
coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs xs = 
    [(x, y) 
    | x <- xs
    , y <- xs
    , x < y
    , gcd x y == 1
    ]