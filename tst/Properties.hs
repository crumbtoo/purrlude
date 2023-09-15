{-# LANGUAGE ConstraintKinds #-}
module Properties where
--------------------------------------------------------------------------------
import           Test.QuickCheck
--------------------------------------------------------------------------------

type EqArbShow a = (Eq a, Arbitrary a, Show a)
type Law = (String, Property)

associativity :: (EqArbShow a) => (a -> a -> a) -> Law
associativity f = ("Associativity", p)
    where p = property $ \a b c ->
            a `f` (b `f` c) == (a `f` b) `f` c

commutativity :: (EqArbShow a) => (a -> a -> a) -> Law
commutativity f = ("Commutativity", p)
    where p = property $ \a b ->
            a `f` b == b `f` a

leftIdentity :: (EqArbShow a) => (a -> a -> a) -> a -> Law
leftIdentity f e = ("Left Identity", p)
    where p = property $ \a ->
            e `f` a == a

rightIdentity :: (EqArbShow a) => (a -> a -> a) -> a -> Law
rightIdentity f e = ("Right Identity", p)
    where p = property $ \a ->
            a `f` e == a

identity :: (EqArbShow a) => (a -> a -> a) -> a -> Law
identity f e = ("Identity", p)
    where p = property $ \a ->
            a `f` e == a && e `f` a == a

leftDistributivity :: (EqArbShow a, EqArbShow b)
                   => (a -> b -> b) -> (b -> b -> b) -> Law
leftDistributivity f g = ("Left Distributivity", p)
    where p = property $ \a b c ->
            a `f` (b `g` c) == (a `f` b) `g` (a `f` c)

rightDistributivity :: (EqArbShow a, EqArbShow b)
                    => (b -> a -> b) -> (b -> b -> b) -> Law
rightDistributivity f g = ("Right Distributivity", p)
    where p = property $ \a b c ->
            (b `g` c) `f` a == (b `f` a) `g` (c `f` a)

distributivity :: (EqArbShow a) => (a -> a -> a) -> (a -> a -> a) -> Law
distributivity f g = ("Distributivity", p)
    where p = property $ \a b c ->
            a `f` (b `g` c) == (a `f` b) `g` (a `f` c)
            && (b `g` c) `f` a == (b `f` a) `g` (c `f` a)

annihilation :: (EqArbShow a) => (a -> a -> a) -> a -> Law
annihilation f z = ("Annihilation", p)
    where p = property $ \a ->
            a `f` z == z && z `f` a == z


