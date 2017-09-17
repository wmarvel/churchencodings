{-# LANGUAGE RankNTypes #-}

module Fun.Types where

-- Looking at paper
-- https://ifl2014.github.io/submissions/ifl2014_submission_13.pdf
type Church = forall a. (a -> a) -> a -> a

-- Small utility to print as Num
cToNum :: (Num a) => Church -> a
cToNum n = n (+1) 0

-- Boolean functions
ftrue :: a -> a -> a
ftrue t f = t

ffalse :: a -> a -> a
ffalse t f = f

fcond :: (a -> b -> c) -> a -> b -> c
fcond c t e = c t e

fand :: (b -> (a -> a -> a) -> c) -> b -> c
fand x y = x y ffalse

-- pairs as functions
fpair :: a -> b -> (a -> b -> c) -> c
fpair x y p = p x y

ffst :: ((a -> b -> a) -> c) -> c
ffst p = p (\x y -> x)

fsnd :: ((d -> e -> e) -> f) -> f
fsnd p = p (\x y -> y)

-- This only works if the pair has the same type in both slots 
fswap' p = fpair (fsnd p) (ffst p)

czero :: Church
czero = \f x -> x

cone :: Church
cone = \f x -> f x

csucc :: Church -> Church
csucc n = \f x -> f (n f x)

cplus :: Church -> Church -> Church
cplus n m = \f x -> n f (m f x)

cmult :: Church -> Church -> Church
cmult n m = \f x -> m (n f) x

cexp :: Church -> Church -> Church
cexp m n = n m
