{-# LANGUAGE Safe, TypeFamilies #-}

module Data.Logic.Choice (
    Choice,

    -- *Construction
    choice, choice',

    -- *Deconstruction
    left, right,

    -- *Utilities
    swap
) where

import Control.Monad.Predicate
import Data.Logic.Atom
import Data.Logic.Term
import Data.Logic.Var

import Control.Applicative ((<$>), (<|>))

-- |A term that can be one of two types.
data Choice a b s = A (Var a s) | B (Var b s)

instance (Term a, Term b) => Term (Choice a b) where
    type Collapse (Choice a b) = Either (Collapse a) (Collapse b)

    collapse (A x) = Left <$> collapse x
    collapse (B x) = Right <$> collapse x

    unify (A x) (A y) = unify x y
    unify (B x) (B y) = unify x y
    unify _ _ = false

    occurs v (A x) = occurs v x
    occurs v (B x) = occurs v x

-- |Constructs a choice.
choice :: (Term a, Term b) => Either (Var a s) (Var b s) -> Var (Choice a b) s
choice = bind . either A B

-- |Constructs a choice of atoms.
choice' :: (Eq a, Eq b) => Either a b -> Var (Choice (Atom a) (Atom b)) s
choice' = bind . either (A . atom) (B . atom)

-- |@left c x@ instantiates its arguments such that @c@ is a left choice containing @x@.
left :: (Term a, Term b) => Var (Choice a b) s -> Var a s -> Predicate s ()
left c x = c `is` choice (Left x)

-- |@right c x@ instantiates its arguments such that @c@ is a right choice containing @x@.
right :: (Term a, Term b) => Var (Choice a b) s -> Var b s -> Predicate s ()
right c x = c `is` choice (Right x)

-- |@swap x y@ instantiates its arguments such that @x@ and
-- @y@ have opposite value types, but contain the same value.
swap :: (Term a, Term b) => Var (Choice a b) s -> Var (Choice b a) s -> Predicate s ()
swap x y =
    do  z <- auto
        left x z
        right y z
    <|>
    do  z <- auto
        right x z
        left y z
