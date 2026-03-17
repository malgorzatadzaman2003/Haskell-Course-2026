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

-- Exercise 7: Integer Power with Bang Patterns
pow :: Int -> Int -> Int
pow _ 0 = 1
pow 0 _ = 0
power b e = go 1 e
  where
    go :: Int -> Int -> Int
    go !acc 0 = acc
    go !acc n = go (acc * b) (n - 1)

-- Exercise 8: Running Maximum: seq vs. Bang Patterns
listMaxSeq :: [Int] -> Int
listMaxSeq [] = error "Empty list"
listMaxSeq (x:xs) = go x xs
  where
    go acc[] = acc -- acc-current max, if no left return
    go acc (y:ys) = -- if there are left, compare with current max and update, then continue with rest 
        let newAcc = max acc y
        in newAcc `seq` go newAcc ys

listMaxBang :: [Int] -> Int
listMaxBang [] = error "Empty list"
listMaxBang (x:xs) = go x xs
  where
    go !acc [] = acc 
    go !acc (y:ys) = go (max acc y) ys  


-- Exercise 9: Infinite Prime Stream
primes :: [Int]
primes = sieve [2..] 

isPrimeInfinite :: Int -> Bool
isPrimeInfinite n 
    | n < 2     = False
    | otherwise = go primes
  where
    go (p:ps)
        | p * p > n = True -- primes up to sqrt(n) and divisor not found, then n is prime
        | n `mod` p == 0 = False -- n divisible by p, then not prime
        | otherwise = go ps -- check next prime
    go [] = False

-- Exercise 10: Strict Accumulation and Space Leaks

-- (a) Non-strict version
mean :: [Double] -> Double
mean xs =
  let (s, n) = go xs 0 0
  in s / fromIntegral n
  where
    go [] sumX count = (sumX, count)
    go (y:ys) sumX count = go ys (sumX + y) (count + 1)

-- (b) Strict version
meanStrict :: [Double] -> Double
meanStrict xs =
  let (s, n) = go xs 0 0
  in s / fromIntegral n
  where
    go [] !sumX !count = (sumX, count)
    go (y:ys) !sumX !count = go ys (sumX + y) (count + 1)

-- (c) Strict for mean and variance
meanVariance :: [Double] -> (Double, Double)
meanVariance xs =
  let (s, s2, n) = go xs 0 0 0
      mu = s / fromIntegral n
      var = s2 / fromIntegral n - mu * mu
  in (mu, var)
  where
    go [] !sumX !sumX2 !count = (sumX, sumX2, count)
    go (y:ys) !sumX !sumX2 !count =
      go ys (sumX + y) (sumX2 + y * y) (count + 1)
