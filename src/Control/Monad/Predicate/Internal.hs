{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes, Trustworthy, TypeFamilies #-}

module Control.Monad.Predicate.Internal (
    Predicate,
    Term (..), solve, solveAll,
    Var, auto, bind
) where

import Control.Applicative (Alternative, Applicative, (<$>))
import Control.Monad (MonadPlus)
import Control.Monad.Logic (LogicT (..), observeAllT, observeT, lift)
import Control.Monad.Logic.Class (MonadLogic)
import Control.Monad.ST.Lazy (ST, runST)
import Data.STRef.Lazy (STRef, newSTRef, readSTRef, writeSTRef)

-- |Describes a computation that supports backtracking and unification.
newtype Predicate s a = Predicate { unPredicate :: LogicT (ST s) a }
  deriving (Alternative, Applicative, Functor, Monad, MonadLogic, MonadPlus)

readRef :: STRef s a -> Predicate s a
readRef = Predicate . lift . readSTRef

writeRef :: STRef s a -> a -> Predicate s ()
writeRef r x = Predicate $ LogicT $ \s f -> do
    -- Save current state.
    x' <- readSTRef r

    writeSTRef r x

    -- Restore previous state on failure.
    s () (writeSTRef r x' >> f)

-- |A term is a value that supports unification.
class Term a where
    -- |The type of value to return when a predicate involving this term is solved.
    type Collapse a

    -- |Collapses a fully instantiated value to a more user-friendly one.
    -- Throws an exception if the value is not fully instantiated.
    collapse :: a s -> Predicate s (Collapse a)

    -- |Unifies two values of the same type.
    unify :: a s -> a s -> Predicate s ()

-- |Takes a predicate that returns a fully instantiated term, and collapses the
-- return value. If the term is not fully instantiated, this function fails.
solve :: Term a => (forall s. Predicate s (Var a s)) -> Collapse a
solve p = runST $ observeT $ unPredicate $ p >>= collapse

-- |Takes a predicate that returns a fully instantiated term, and collapses all
-- possible results. If the term is not fully instantiated, this function
-- returns no results.
solveAll :: Term a => (forall s. Predicate s (Var a s)) -> [Collapse a]
solveAll p = runST $ observeAllT $ unPredicate $ p >>= collapse

-- |A unifiable and possibly uninstantiated value.
data Var a s
    = Reference (STRef s (Maybe (Var a s)))
    | Binding (a s)

-- |An uninstantiated `Var`.
auto :: Term a => Predicate s (Var a s)
auto = Predicate $ lift $ Reference <$> newSTRef Nothing

-- |Lifts a term into a `Var`.
bind :: Term a => a s -> Var a s
bind = Binding

instance Term a => Term (Var a) where
    type Collapse (Var a) = Collapse a

    collapse (Binding x) = collapse x
    collapse (Reference x) =
        readRef x >>=
        maybe (error "Value is not sufficiently instantiated.") collapse

    unify (Binding x) (Binding y) = unify x y
    unify (Reference x) (Reference y) | x == y = return ()
    unify (Reference x) y = readRef x >>= maybe (writeRef x (Just y)) (unify y)
    unify x y = unify y x
