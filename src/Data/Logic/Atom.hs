{-# LANGUAGE Safe, TypeFamilies #-}

module Data.Logic.Atom (
    Atom, atom, unit
) where

import Control.Monad.Predicate
import Data.Logic.Term
import Data.Logic.Var

-- |A constant term.
newtype Atom a s = Atom a

instance Eq a => Term (Atom a) where
    type Collapse (Atom a) = a
    collapse (Atom x) = return x
    unify (Atom x) (Atom y) = bool (x == y)
    occurs _ _ = return False

-- |Constructs an atom.
atom :: Eq a => a -> Var (Atom a) s
atom = bind . Atom

-- |Synonym for @atom ()@.
unit :: Var (Atom ()) s
unit = atom ()
