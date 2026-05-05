newtype Reader r a = Reader { runReader :: r -> a }
-- ^ runReader executes a Reader computation by supplying an environment `r`
--   and returning a result of type `a`.


-- Exercise 1: Functor, Applicative, and Monad instances

instance Functor (Reader r) where
  -- fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader g) = Reader (\env -> f (g env))
  

instance Applicative (Reader r) where
  -- pure   :: a -> Reader r a
  pure x = Reader (\_ -> x)
  -- liftA2 :: (a -> b -> c) -> Reader r a -> Reader r b -> Reader r c
  liftA2 f (Reader ra) (Reader rb) =
        Reader (\env -> f (ra env) (rb env))

instance Monad (Reader r) where
  -- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (>>=) = undefined