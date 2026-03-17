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
    | n < 4 || odd n = [] -- Goldbach's conjecture applies to even integers greater equal than 4
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

-- Exercise 4: Matrix Multiplication
matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul a b =
    [[sum [ a !! i !! k * b !! k !! j | k <- [0 .. p-1] ] 
     | j <- [0 .. n-1] 
     ] 
    | i <- [0 .. m-1] 
    ]
    where
        m = length a
        p = length (head a)
        n = length (head b)

-- Exercise 5: Permutations

--helper function: remove first occurrence of given element from a list, to avoid reusing elements when generating permutations
removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst y (x:xs)
    | y == x    = xs
    | otherwise = x : removeFirst y xs       

--permutations function: 
-- Idea: for each element, fix it as the first and recursively construct smaller permutations.

permutations :: Eq a => Int -> [a] -> [[a]]
permutations 0 _ = [[]] 
permutations _ [] = []
permutations k xs = 
    [ x : perm 
    | x <- xs
    , let remaining = removeFirst x xs
    , perm <- permutations (k - 1) remaining
    ]

-- Exercise 6: Hamming Numbers
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x < y     = x : merge xs (y:ys) -- we need sorted output hence comparisions 
    | x > y     = y : merge (x:xs) ys
    | otherwise = x : merge xs ys  

-- Idea: -- take every hamming number(starting from 1) and multiply by 2, then merge with the rest(rest means do the same with 3 and 5)
hamming :: [Int]
hamming = 1 : merge (map (*2) hamming) 
                    (merge (map (*3) hamming) 
                    (map (*5) hamming))        