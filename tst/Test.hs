{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Prelude hiding ((+), (*))
import Algebra.Semiring
import Test.QuickCheck

-- these tests are kinda useless. i haven't used quickcheck before lol. ideally
-- i could somehow test all insances of a class...

semiring_additionAssociativity :: (Semiring a) => a -> a -> a -> Bool
semiring_additionAssociativity a b c = (a + b) + c == a + (b + c)

semiring_additionCommutivity :: (Semiring a) => a -> a -> Bool
semiring_additionCommutivity a b = a + b == b + a

semiring_additionIdentity :: (Semiring a) => a -> Bool
semiring_additionIdentity a = a + zero == a && zero + a == a

semiring_multiplicationAssociativity :: (Semiring a) => a -> a -> a -> Bool
semiring_multiplicationAssociativity a b c = (a * b) * c == a * (b * c)

-- TH thing
return []

main :: IO ()
main = do
    $(polyQuickCheck 'semiring_additionAssociativity)
    $(polyQuickCheck 'semiring_additionCommutivity)
    $(polyQuickCheck 'semiring_additionIdentity)
    $(polyQuickCheck 'semiring_multiplicationAssociativity)

