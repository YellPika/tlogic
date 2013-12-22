{-# LANGUAGE Safe, TypeFamilies #-}

module Data.Logic.Nat (
    Nat,

    -- * Construction
    nat, zero, next,

    -- * Destruction
    prev,

    -- * Utilities
    add, sub, mult, divd, invert
) where

import Control.Monad.Predicate
import Data.Logic.Term
import Data.Logic.Var

import Control.Applicative ((<|>), (<$>))

-- |Natural number terms.
data Nat s = Zero | Next (Var Nat s)

instance Term Nat where
    type Collapse Nat = Integer

    collapse Zero     = return 0
    collapse (Next n) = (+ 1) <$> collapse n

    unify Zero     Zero     = true
    unify (Next n) (Next m) = unify n m
    unify _         _       = false

    occurs _ Zero     = return False
    occurs v (Next x) = occurs v x

-- |Constructs a natural number.
nat :: Integer -> Var Nat s
nat n | n == 0    = zero
      | n > 0     = bind $ Next $ nat (n - 1)
      | otherwise = error $ "Invalid natural, '" ++ show n ++ "'"

-- |The `Nat` representation of zero.
zero :: Var Nat s
zero = bind Zero

-- |The `Nat` representation of one.
one :: Var Nat s
one = next zero

-- |@next n@ computes @n + 1@.
next :: Var Nat s -> Var Nat s
next = bind . Next

-- |@prev x y@ unifies its arguments such that @x = y + 1@.
prev :: Var Nat s -> Var Nat s -> Predicate s ()
prev x y = x `is` next y

-- |@add x y z@ unifies its arguments such that @x + y = z@.
add :: Var Nat s -> Var Nat s -> Var Nat s -> Predicate s ()
add x y z =
    do  x `is` zero
        y `is` z
    <|>
    do  x' <- from (prev x)
        add x' (next y) z

-- |@sub x y z@ unifies its arguments such that @x - y = z@.
sub :: Var Nat s -> Var Nat s -> Var Nat s -> Predicate s ()
sub = invert add

-- |@mult x y z@ unifies its arguments such that @x * y = z@.
mult :: Var Nat s -> Var Nat s -> Var Nat s -> Predicate s ()
mult x y z =
    do  x `is` zero
        y `is` zero
        z `is` zero
    <|>
    do  x `is` zero <|> y `is` zero
        z `is` zero
    <|>
    do  x `is` one
        y `is` z
    <|>
    do  y `is` one
        x `is` z
    <|>
    do  x' <- from (prev x)
        z' <- from (sub z x)
        mult x' y z'

-- |@divd x y z@ unifies its arguments such that @x / y = z@.
divd :: Var Nat s -> Var Nat s -> Var Nat s -> Predicate s ()
divd = invert mult

-- |Inverts a binary operation. For example, @sub = invert add@.
invert ::
    (Var Nat s -> Var Nat s -> Var Nat s -> Predicate s ()) ->
     Var Nat s -> Var Nat s -> Var Nat s -> Predicate s ()
invert f x y z = f y z x
