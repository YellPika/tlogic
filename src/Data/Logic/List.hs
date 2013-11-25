{-# LANGUAGE Safe, TypeFamilies #-}

module Data.Logic.List (
    List,

    -- *Construction
    list, list', nil, cons,

    -- *Deconstruction
    decons, first, rest,

    -- *Utilities
    member, delete, append,
) where

import Control.Monad.Predicate
import Data.Logic.Atom
import Data.Logic.Term
import Data.Logic.Var

import Control.Applicative ((<$>), (<*>), (<|>))

-- |A list of terms.
data List a s = Nil | Cons !(Var a s) !(Var (List a) s)

instance Term a => Term (List a) where
    type Collapse (List a) = [Collapse a]

    collapse Nil = return []
    collapse (Cons x xs) = (:) <$> collapse x <*> collapse xs

    unify Nil Nil = true
    unify (Cons x xs) (Cons y ys) = unify x y >> unify xs ys
    unify _ _ = false

-- |Creates a new term list.
list :: Term a => [Var a s] -> Var (List a) s
list [] = bind Nil
list (x:xs) = bind $ Cons x $ list xs

-- |Creates a list of atoms.
list' :: Eq a => [a] -> Var (List (Atom a)) s
list' = list . map atom

-- |The empty list.
nil :: Term a => Var (List a) s
nil = bind Nil

-- |@cons x xs@ constructs a list with @x@ as the first element, and @xs@ as the rest.
cons :: Term a => Var a s -> Var (List a) s -> Var (List a) s
cons = (bind .) . Cons

-- |@decons zs x xs@ instantiates its arguments such that @zs@ is @x:xs@.
decons :: Term a => Var (List a) s -> Var a s -> Var (List a) s -> Predicate s ()
decons zs x xs = zs `is` cons x xs

-- |@first xs x@ instantiates its arguments such that @xs@ is @x:_@.
first :: Term a => Var (List a) s -> Var a s -> Predicate s ()
first ys x = auto >>= decons ys x

-- |@rest xs ys@ instantiates its arguments such that @xs@ is @_:ys@.
rest :: Term a => Var (List a) s -> Var (List a) s -> Predicate s ()
rest ys xs = auto >>= flip (decons ys) xs

-- |@member x xs@ instantiates its arguments such that @x@ is an element of @xs@.
member :: Term a => Var a s -> Var (List a) s -> Predicate s ()
member x xs = do
    (x', xs') <- from2 $ decons xs
    x `is` x' <|> member x xs'

-- |@delete x xs ys@ instantiates its arguments such that
-- @ys@ contains every element in @xs@ that is not @x@.
delete :: Term a => Var a s -> Var (List a) s -> Var (List a) s -> Predicate s ()
delete x xs ys =
    do  xs `is` nil
        ys `is` nil
    <|>
    do  (x', xs') <- from2 $ decons xs
        (x `is` x' >> delete x xs' ys) <|>
            (from (decons ys x') >>= delete x xs')

-- |@append xs ys zs@ instantiates its arguments such that @zs@ is @xs ++ ys@.
append :: Term a => Var (List a) s -> Var (List a) s -> Var (List a) s -> Predicate s ()
append xs ys zs =
    do  xs `is` nil
        ys `is` zs
    <|>
    do  (x', xs') <- from2 $ decons xs
        zs' <- from $ decons zs x'
        append xs' ys zs'
