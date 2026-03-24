-- Exercise 1: Functor for Sequence

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)
    deriving Show

instance Functor Sequence where
    fmap _ Empty = Empty
    fmap f (Single x) = Single (f x)
    fmap f (Append l r) = Append (fmap f l) (fmap f r)

