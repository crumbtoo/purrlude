{-# LANGUAGE TemplateHaskell, RankNTypes, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE ConstraintKinds, KindSignatures #-}
module Main (main) where
--------------------------------------------------------------------------------
import           Prelude                            hiding ( Semigroup, (<>)
                                                           , Monoid, mempty
                                                           )
import           Data.Proxy
import           Algebra.Semigroup
import           Algebra.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Classes            ( Laws(..), lawsCheck
                                                    , lawsCheckMany
                                                    )
import           Text.Printf                        (printf)
import           Data.Typeable                      (typeRep, Typeable)
import           Data.Foldable                      (traverse_)
--------------------------------------------------------------------------------

-- temporary; don't feel like adding test-invariant rn

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

--------------------------------------------------------------------------------

type TestableLaws a = (Show a, Arbitrary a, Eq a, Typeable a)

typeName :: (Typeable a) => Proxy a -> String
typeName p = show $ typeRep p

mklaws :: (TestableLaws a) => String -> Proxy a -> [(String, Property)] -> Laws
mklaws tclass p laws = Laws (printf "%s (%s)" tclass (typeName p)) laws

typeLaws :: (TestableLaws a) => Proxy a -> [Proxy a -> Laws] -> IO ()
typeLaws p ls = tests $ fmap ($ p) ls

tests :: [Laws] -> IO ()
tests = traverse_ lawsCheck

--------------------------------------------------------------------------------

semigroupLaws :: forall a. (TestableLaws a, Semigroup a) => Proxy a -> Laws
semigroupLaws p = mklaws "Semigroup" p
    [ associativity @a (<>)
    ]

commutativeSemigroupLaws :: forall a. (TestableLaws a, CommutativeSemigroup a)
                         => Proxy a -> Laws
commutativeSemigroupLaws p = mklaws "CommutativeSemigroup" p
    [ commutativity @a (<>)
    ]

monoidLaws :: forall a. (TestableLaws a, Monoid a) => Proxy a -> Laws
monoidLaws p = mklaws "Monoid" p
    [ leftIdentity @a (<>) mempty
    , rightIdentity @a (<>) mempty
    ]

--------------------------------------------------------------------------------

test_Sum_Int :: IO ()
test_Sum_Int = typeLaws (Proxy :: Proxy (Sum Int))
        [ semigroupLaws
        , commutativeSemigroupLaws
        , monoidLaws
        ]

test_Product_Int :: IO ()
test_Product_Int = typeLaws (Proxy :: Proxy (Product Int))
        [ semigroupLaws
        , commutativeSemigroupLaws
        , monoidLaws
        ]

main :: IO ()
main = do
    test_Sum_Int
    test_Product_Int

