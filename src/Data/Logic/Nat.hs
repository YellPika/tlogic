{-# LANGUAGE Safe, TypeFamilies #-}

module Data.Logic.Nat (
    Nat,

    -- * Construction
    nat, zero, next,

    -- * Destruction
    prev,

    -- * Utilities
    add
) where

import Control.Monad.Predicate
import Data.Logic.Term
import Data.Logic.Var

import Control.Applicative ((<|>), (<$>))

-- |Natural number terms.
data Nat s = Zero | Next (Var Nat s)

instance Term Nat where
    type Collapse Nat = Integer

    collapse Zero = return 0
    collapse (Next n) = (+ 1) <$> collapse n

    unify Zero Zero = true
    unify (Next n) (Next m) = unify n m
    unify _ _ = false

-- |Constructs a natural number.
nat :: Integer -> Var Nat s
nat 0 = zero
nat n | n == 0 = zero
      | n > 0 = bind $ Next $ nat (n - 1)
      | otherwise = error $ "Invalid natural, '" ++ show n ++ "'"

-- |The first natural number.
zero :: Var Nat s
zero = bind Zero

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
