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