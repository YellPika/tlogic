{-# LANGUAGE Safe, TypeFamilies #-}

module Data.Logic.Pair (
    Pair,

    -- *Construction
    pair, pair',

    -- *Deconstruction
    first, second,

    -- *Utilities
    swap
) where

import Control.Monad.Predicate
import Data.Logic.Atom
import Data.Logic.Term
import Data.Logic.Var

import Control.Applicative ((<$>), (<*>))

-- |A pair of terms.
data Pair a b s = Pair (Var a s) (Var b s)

instance (Term a, Term b) => Term (Pair a b) where
    type Collapse (Pair a b) = (Collapse a, Collapse b)
    collapse (Pair x y) = (,) <$> collapse x <*> collapse y
    unify (Pair x y) (Pair x' y') = unify x x' >> unify y y'

-- |Constructs a pair of terms.
pair :: (Term a, Term b) => Var a s -> Var b s -> Var (Pair a b) s
pair x y = bind $ Pair x y

-- |Constructs a pair of atoms.
pair' :: (Eq a, Eq b) => a -> b -> Var (Pair (Atom a) (Atom b)) s
pair' x y = pair (atom x) (atom y)

-- |@first x p@ instantiates its arguments such that @p@ is @(x, _)@.
first :: (Term a, Term b) => Var a s -> Var (Pair a b) s -> Predicate s ()
first x p = do
    y <- auto
    p `is` pair x y

-- |@second x p@ instantiates its arguments such that @p@ is @(_, x)@.
second :: (Term a, Term b) => Var b s -> Var (Pair a b) s -> Predicate s ()
second y p = do
    x <- auto
    p `is` pair x y

-- |@swap p q@ instantiates its arguments such that @p@ is @(x, y)@, and @q@ is @(y, x)@.
swap :: (Term a, Term b) => Var (Pair a b) s -> Var (Pair b a) s -> Predicate s ()
swap x y = do
    (l, r) <- auto2
    x `is` pair l r
    y `is` pair r l
