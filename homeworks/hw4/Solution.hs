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
  Reader ra >>= f =
        Reader (\env ->
            let a = ra env
                Reader rb = f a
            in rb env)

-- Exercise 2: Primitive operations
-- Retrieves the entire environment.
ask   :: Reader r r
ask = Reader (\env -> env)

-- Retrieves a value derived from the environment by applying a projection,
-- e.g. `asks interestRate :: Reader BankConfig Double`.
asks  :: (r -> a) -> Reader r a
asks f = Reader (\env -> f env)

-- Runs a subcomputation in a locally modified environment. The modification
-- is only visible inside the passed Reader — once it returns, the outer
-- environment is restored (conceptually; there is no mutable state, the
-- modified environment simply goes out of scope).
local :: (r -> r) -> Reader r a -> Reader r a
local modify (Reader computation) =
    Reader (\env -> computation (modify env))