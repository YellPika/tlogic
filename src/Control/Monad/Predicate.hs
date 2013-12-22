{-# LANGUAGE Safe #-}

module Control.Monad.Predicate (
    module Control.Monad.Logic,

    Predicate,

    -- * Solving
    solve, solveAll,

    -- * Simple Predicates
    true, false, bool,

    -- * Settings
    OccursCheck (..), withOccursCheck
) where

import Control.Monad.Predicate.Internal

import Control.Monad.Logic
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
