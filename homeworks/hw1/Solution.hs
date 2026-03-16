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