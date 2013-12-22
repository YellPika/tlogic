{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes, Trustworthy, TypeFamilies #-}

module Control.Monad.Predicate.Internal (
    Predicate,
    OccursCheck (..), withOccursCheck,
    Term (..), Unbound, solve, solveAll,
    Var, auto, bind, unbound
) where

import Control.Applicative (Alternative, Applicative, (<$>), empty)
import Control.Monad (MonadPlus, when, guard)
import Control.Monad.Logic (LogicT (..), observeAllT, observeT, lift)
import Control.Monad.Logic.Class (MonadLogic)
import Control.Monad.ST.Lazy (ST, runST)
import Control.Monad.Trans.Reader (ReaderT (..), asks, local)
import Data.Maybe (isJust)
import Data.STRef.Lazy (STRef, newSTRef, readSTRef, writeSTRef)
import Unsafe.Coerce (unsafeCoerce)

-- |Describes how a `Predicate` computation should handle the occurs check.
data OccursCheck
    = Error -- ^The occurs check will throw an exception.
    | Fail  -- ^The occurs check will cause the computation to backtrack.
    | Off   -- ^Do not perform the occurs check. This is the default value.
  deriving (Eq, Show)

-- Why a full blown record? I might add stuff later.
data Settings = Settings {
    occursCheck :: OccursCheck
}

-- |Describes a computation that supports backtracking and unification.
newtype Predicate s a = Predicate { unPredicate :: ReaderT Settings (LogicT (ST s)) a }
  deriving (Alternative, Applicative, Functor, Monad, MonadLogic, MonadPlus)

-- |Executes a predicate with the specified occurs check setting.
--
-- Example:
-- 
-- > withOccursCheck Fail (x `is` cons unit x)
withOccursCheck :: OccursCheck -> Predicate s a -> Predicate s a
withOccursCheck c = Predicate . local (\x -> x { occursCheck = c }) . unPredicate

readRef :: STRef s a -> Predicate s a
readRef = Predicate . lift . lift . readSTRef

writeRef :: STRef s a -> a -> Predicate s ()
writeRef r x = Predicate $ lift $ LogicT $ \s f -> do
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

    -- |@occurs v t@ determines if the unbound variable `v` occurs in the term `a`.
    occurs :: Unbound b s -> a s -> Predicate s Bool

-- |Takes a predicate that returns a fully instantiated term, and collapses the
-- return value. If the term is not fully instantiated, this function fails.
solve :: Term a => (forall s. Predicate s (Var a s)) -> Collapse a
solve p = evalWith observeT (p >>= collapse)

-- |Takes a predicate that returns a fully instantiated term, and collapses all
-- possible results. If the term is not fully instantiated, this function
-- returns no results.
solveAll :: Term a => (forall s. Predicate s (Var a s)) -> [Collapse a]
solveAll p = evalWith observeAllT (p >>= collapse)

evalWith :: (forall s. LogicT (ST s) a -> ST s b) -> (forall s. Predicate s a) -> b
evalWith f p = runST $ f $ runReaderT (unPredicate p) defaultSettings
  where
    defaultSettings = Settings {
        occursCheck = Off
    }

-- |A unifiable and possibly uninstantiated value.
data Var a s
    = Reference (STRef s (Maybe (Var a s)))
    | Binding (a s)

-- |An unbound variable. Used for the occurs check.
newtype Unbound a s = Unbound (STRef s (Maybe (Var a s)))

-- |An uninstantiated `Var`.
auto :: Term a => Predicate s (Var a s)
auto = Predicate $ lift $ lift $ Reference <$> newSTRef Nothing

-- |Lifts a term into a `Var`.
bind :: Term a => a s -> Var a s
bind = Binding

-- |@unbound x@ succeeds if and only if @x@ is an unbound variable.
unbound :: Term a => Var a s -> Predicate s ()
unbound (Binding   _) = empty
unbound (Reference x) = do
    value <- readRef x
    guard (isJust value)
    return ()

instance Term a => Term (Var a) where
    type Collapse (Var a) = Collapse a

    collapse (Binding   x) = collapse x
    collapse (Reference x) = do
        value <- readRef x
        case value of
            Nothing -> error "Value is not sufficiently instantiated."
            Just x' -> collapse x'

    unify v1 v2 = do
        x <- reduce v1
        y <- reduce v2
        unifyReduced x y
      where
        unifyReduced (Binding   x) (Binding   y)          = unify x y
        unifyReduced (Reference x) (Reference y) | x == y = return ()
        unifyReduced (Reference x) y                      = unifyRef x y
        unifyReduced x             (Reference y)          = unifyRef y x

        unifyRef x y = do
            check <- Predicate (asks occursCheck)
            when (check /= Off) $ do
                test <- occurs (Unbound x) y
                when test $ do
                    -- Don't throw the error if we're only supposed to backtrack.
                    guard (check /= Fail)
                    error "Occurs check failed."

            writeRef x (Just y)

        -- Takes a Var and returns a Var such that the output is
        -- either a Reference to Nothing, or a Binding.
        --
        -- reduce (Reference (Just x)) = reduce x
        -- reduce x                    = x
        reduce v@(Reference x) = do
            result <- readRef x
            case result of
                Nothing -> return v
                Just x' -> reduce x'
        reduce v = return v

    occurs x@(Unbound ref) = occursIn
      where
        occursIn (Binding   y) = occurs x y
        occursIn (Reference y)
            | unsafeCoerce ref == y = return True
            | otherwise             = do
                value <- readRef y
                case value of
                    Nothing -> return False
                    Just y' -> occursIn y'
