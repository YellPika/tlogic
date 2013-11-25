{-# LANGUAGE Safe #-}

module Control.Monad.Predicate (
    Predicate, true, false, bool, solve, solveAll
) where

import Control.Monad.Predicate.Internal
import Control.Applicative (empty)

-- |A predicate computation that always succeeds.
true :: Predicate s ()
true = return ()

-- |A predicate computation that always fails.
false :: Predicate s ()
false = empty

-- |@bool x@ succeeds if @x@ is true, and fails otherwise.
bool :: Bool -> Predicate s ()
bool True = true
bool False = false