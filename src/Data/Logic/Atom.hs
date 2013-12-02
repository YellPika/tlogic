{-# LANGUAGE Safe, TypeFamilies #-}

module Data.Logic.Atom (
    Atom, atom
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

-- |Constructs an atom.
atom :: Eq a => a -> Var (Atom a) s
atom = bind . Atom
