{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Prelude hiding ((+), (*))
import Algebra.Semiring
import Test.QuickCheck

-- these tests are kinda useless. i haven't used quickcheck before lol. ideally
-- i could somehow test all insances of a class...

additionAssociativity :: forall a. (Semiring a) => a -> a -> a -> Bool
additionAssociativity a b c = (a + b) + c == a + (b + c)

additionCommutivity :: (Semiring a) => a -> a -> Bool
additionCommutivity a b = a + b == b + a

additionIdentity :: (Semiring a) => a -> Bool
additionIdentity a = a + zero == a && zero + a == a

multiplicationAssociativity :: (Semiring a) => a -> a -> a -> Bool
multiplicationAssociativity a b c = (a * b) * c == a * (b * c)

-- TH thing
return []

main :: IO ()
main = do
    $(polyQuickCheck 'semiring_additionAssociativity)
    $(polyQuickCheck 'semiring_additionCommutivity)
    $(polyQuickCheck 'semiring_additionIdentity)
    $(polyQuickCheck 'semiring_multiplicationAssociativity)

