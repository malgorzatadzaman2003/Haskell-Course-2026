import Data.Foldable (toList) -- for seqToList

-- Exercise 1: Functor for Sequence

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)
    deriving Show

instance Functor Sequence where
    fmap _ Empty = Empty
    fmap f (Single x) = Single (f x)
    fmap f (Append l r) = Append (fmap f l) (fmap f r)

-- Exercise 2: Foldable for Sequence

instance Foldable Sequence where
    foldMap :: Monoid m => (a -> m) -> Sequence a -> m
    foldMap _ Empty = mempty
    foldMap f (Single x) = f x
    foldMap f (Append l r) = foldMap f l <> foldMap f r

-- seqToList

seqToList :: Sequence a -> [a]
seqToList = toList

-- seqLength

seqLength :: Sequence a -> Int
seqLength = length

-- Exercise 3: Semigroup and Monoid for Sequence

instance Semigroup (Sequence a) where
    Empty <> s = s
    s <> Empty = s
    s1 <> s2 = Append s1 s2

instance Monoid (Sequence a) where
    mempty = Empty

-- Exercise 4: Tail Recursion and Sequence Search

tailElem :: Eq a => a -> Sequence a -> Bool
tailElem x seq0 = go [seq0] -- stack containing one item, the whole sequence
  where
    go [] = False
    go (Empty : rest) = go rest
    go (Single y : rest)
        | x == y    = True -- curr element matches x, return True
        | otherwise = go rest
    go (Append l r : rest) = go (l : r : rest) -- decompose append and continue with both parts

-- Exercise 5: Tail Recursion and Sequence Flatten

tailToList :: Sequence a -> [a]
tailToList seq0 = reverse (go [seq0] []) 
  where
    go [] acc = acc
    go (Empty : rest) acc = go rest acc
    go (Single x : rest) acc = go rest (x : acc) -- prepend current element to accumulator( adds to the fromt)
    go (Append l r : rest) acc = go (l : r : rest) acc 

-- Exercise 6: Tail Recursion and Reverse Polish Notation

data Token = TNum Int | TAdd | TSub | TMul | TDiv
    deriving Show

tailRPN :: [Token] -> Maybe Int
tailRPN tokens = go tokens []
  where
    go [] [result] = Just result -- no tokens left, stack = one value
    go [] _ = Nothing 
    go (TNum n : rest) stack = go rest (n : stack) -- push number n onto stack
    go (op : rest) (y : x : stack) = case op of
        TAdd -> go rest ((x + y) : stack)
        TSub -> go rest ((x - y) : stack)
        TMul -> go rest ((x * y) : stack)
        TDiv -> if y == 0 then Nothing -- division by 0
                else go rest ((x `div` y) : stack)
    go _ _ = Nothing -- malformed expressions 

-- Exercise 7: Expressing functions via foldr and foldl
-- (a)
myReverse :: [a] -> [a] 
myReverse = foldl (flip (:)) [] 

-- (b)
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p = foldr step []
  where
    step x acc
        | p x       = x : acc -- if x satisfies p, include it in the result
        | otherwise = []     

-- (c)
decimal :: [Int] -> Int 
decimal = foldl (\acc d -> acc * 10 + d) 0

-- Exercise 8: Run-length encoding via folds
-- (a)
encode :: Eq a => [a] -> [(a, Int)]
encode = foldr step []
  where
    step x [] = [(x, 1)] 
    step x ((y, n) : rest) 
        | x == y    = (y, n + 1) : rest -- equal: increase count of the run 
        | otherwise = (x, 1) : (y, n) : rest -- not equal: start a new run for x, keep the existing run for y

-- (b)
decode :: [(a, Int)] -> [a]
decode = foldr step []
  where    step (x, n) acc = replicate n x ++ acc -- replicate x n times and concatenate with the accumulated result