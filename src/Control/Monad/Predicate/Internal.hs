{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes, Trustworthy, TypeFamilies #-}

module Control.Monad.Predicate.Internal (
    Predicate,
    Term (..), solve, solveAll,
    Var, auto, bind, unbound
) where

import Control.Applicative (Alternative, Applicative, (<$>), empty)
import Control.Monad (MonadPlus)
import Control.Monad.Logic (LogicT (..), observeAllT, observeT, lift)
import Control.Monad.Logic.Class (MonadLogic)
import Control.Monad.ST.Lazy (ST, runST)
import Data.STRef.Lazy (STRef, newSTRef, readSTRef, writeSTRef)
import Unsafe.Coerce (unsafeCoerce)

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

    -- |`occurs v t` determines if the unbound variable `v` occurs in the term `a`.
    occurs :: Unbound b s -> a s -> Predicate s Bool

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
data Var a s = Reference (STRef s (Maybe (Var a s))) | Binding (a s)

-- |An unbound variable. Used for the occurs check.
newtype Unbound a s = Unbound (STRef s (Maybe (Var a s)))

-- |An uninstantiated `Var`.
auto :: Term a => Predicate s (Var a s)
auto = Predicate $ lift $ Reference <$> newSTRef Nothing

-- |Lifts a term into a `Var`.
bind :: Term a => a s -> Var a s
bind = Binding

-- |@unbound x@ succeeds if and only if @x@ is an unbound variable.
unbound :: Term a => Var a s -> Predicate s ()
unbound (Binding _) = empty
unbound (Reference x) = readRef x >>= maybe empty (return . const ())

instance Term a => Term (Var a) where
    type Collapse (Var a) = Collapse a

    collapse (Binding x) = collapse x
    collapse (Reference x) =
        readRef x >>=
        maybe (error "Value is not sufficiently instantiated.") collapse

    unify v1 v2 = do
        x <- reduce v1
        y <- reduce v2
        unify' x y
      where
        unify' (Binding x) (Binding y) = unify x y
        unify' (Reference x) (Reference y) | x == y = return ()
        unify' (Reference x) y = do
            test <- occurs (Unbound x) y
            if test
                then error "Occurs check failed."
                else writeRef x (Just y)
        unify' x y = unify' y x

        reduce v@(Binding _) = return v
        reduce v@(Reference x) = do
            result <- readRef x
            case result of
                Nothing -> return v
                Just x' -> reduce x'

    occurs x (Binding y) = occurs x y
    occurs x@(Unbound ref) (Reference y)
        | unsafeCoerce ref == y = return True
        | otherwise = readRef y >>= maybe (return False) (occurs x)
