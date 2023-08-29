{-# LANGUAGE TemplateHaskell, RankNTypes, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE ConstraintKinds, KindSignatures #-}
module Main (main) where
--------------------------------------------------------------------------------
import           Prelude                            hiding (Semigroup, (<>))
import           Data.Proxy
import           Algebra.Semigroup
import           Test.QuickCheck
import           Test.QuickCheck.Classes            (Laws(..), lawsCheck)
import           Text.Printf                        (printf)
import           Data.Typeable                      (typeRep, Typeable)
import           Data.Foldable                      (traverse_)
--------------------------------------------------------------------------------

-- temporary; don't feel like adding test-invariant rn

type EqArbShow a = (Eq a, Arbitrary a, Show a)

-- associative :: (Eq a) => (a -> a -> a) -> a -> a -> a -> Bool
-- associative f a b c = a `f` (b `f` c) == (a `f` b) `f` c
associative :: (EqArbShow a) => (a -> a -> a) -> Property
associative f = property $ \a b c -> a `f` (b `f` c) == (a `f` b) `f` c

commutative :: (EqArbShow a) => (a -> a -> a) -> Property
commutative f = property $ \a b -> a `f` b == b `f` a



--------------------------------------------------------------------------------

type TestableLaws a = (Show a, Arbitrary a, Eq a, Typeable a)

mklaws :: (TestableLaws a) => String -> Proxy a -> [(String, Property)] -> Laws
mklaws tclass p laws = Laws (printf "%s (%s)" tclass (show $ typeRep p)) laws

semigroupLaws :: forall a. (TestableLaws a, Semigroup a) => Proxy a -> Laws
semigroupLaws p = mklaws "Semigroup" p
    [ ("Associativity",        associative @a (<>))
    ]

semigroupTests :: IO ()
semigroupTests = traverse_ lawsCheck
    [ semigroupLaws (Proxy :: Proxy (Sum Int))
    , semigroupLaws (Proxy :: Proxy (Product Int))
    ]

main :: IO ()
main = do
    semigroupTests
