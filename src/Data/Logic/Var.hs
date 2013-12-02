{-# LANGUAGE Safe #-}

module Data.Logic.Var (
    Var,

    -- * Uninstantiated variable creation
    auto, auto2, auto3, auto4, auto5,
    from, from2, from3,

    -- * Binding
    bind,

    -- * Unification and instantiation
    is, inst
) where

import Control.Monad.Predicate.Internal
import Control.Applicative ((<*>), (<$>))

-- |Two value version of `auto`.
auto2 :: (Term a, Term b) => Predicate s (Var a s, Var b s)
auto2 = (,) <$> auto <*> auto

-- |Three value version of `auto`.
auto3 :: (Term a, Term b, Term c) => Predicate s (Var a s, Var b s, Var c s)
auto3 = (,,) <$> auto <*> auto <*> auto

-- |Four value version of `auto`.
auto4 :: (Term a, Term b, Term c, Term d) => Predicate s (Var a s, Var b s, Var c s, Var d s)
auto4 = (,,,) <$> auto <*> auto <*> auto <*> auto

-- |Five value version of `auto`.
auto5 :: (Term a, Term b, Term c, Term d, Term e) => Predicate s (Var a s, Var b s, Var c s, Var d s, Var e s)
auto5 = (,,,,) <$> auto <*> auto <*> auto <*> auto <*> auto

-- |Inverts a predicate function such that the argument is returned.
--
-- >  zs <- auto
-- >  append xs ys zs
--
-- can be shortened to
--
-- >  zs <- from $ append xs ys
from :: Term a => (Var a s -> Predicate s ()) -> Predicate s (Var a s)
from f = do
    x <- auto
    f x
    return x

-- |Two value version of `from`.
from2 :: (Term a, Term b) => (Var a s -> Var b s -> Predicate s ()) -> Predicate s (Var a s, Var b s)
from2 f = do
    (x, y) <- auto2
    f x y
    return (x, y)

-- |Three value version of `from`.
from3 :: (Term a, Term b, Term c) => (Var a s -> Var b s -> Var c s -> Predicate s ()) -> Predicate s (Var a s, Var b s, Var c s)
from3 f = do
    (x, y, z) <- auto3
    f x y z
    return (x, y, z)

-- |A synonym for unify, specifically for `Var`s.
-- Meant to be used in infix form.
--
-- >  x `is` y
is :: Term a => Var a s -> Var a s -> Predicate s ()
is = unify

-- |A synonym for collapse, specifically for `Var`s.
--
-- >  value <- inst term
-- >  return $ atom $ doSomethingTo value
inst :: Term a => Var a s -> Predicate s (Collapse a)
inst = collapse
