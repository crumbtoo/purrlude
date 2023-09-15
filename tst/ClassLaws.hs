{-# LANGUAGE ConstraintKinds, KindSignatures, RankNTypes #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
module ClassLaws where
--------------------------------------------------------------------------------
import           Prelude                            hiding ( Semigroup, (<>)
                                                           , Monoid, mempty
                                                           , (+), (*)
                                                           )
import           Test.QuickCheck
import           Test.QuickCheck.Classes            ( Laws(..), lawsCheck
                                                    , lawsCheckMany
                                                    )
import           Data.Proxy
import           Data.Typeable                      (typeRep, Typeable)
import           Data.Foldable                      (traverse_)
import           Text.Printf                        (printf)

import           Algebra.Semiring
import           Algebra.Monoid

import Properties
--------------------------------------------------------------------------------

type TestableLaws a = (Show a, Arbitrary a, Eq a, Typeable a)

typeName :: (Typeable a) => Proxy a -> String
typeName p = show $ typeRep p

mklaws :: (TestableLaws a) => String -> Proxy a -> [(String, Property)] -> Laws
mklaws tclass p laws = Laws (printf "%s (%s)" tclass (typeName p)) laws

typeLaws :: (TestableLaws a) => Proxy a -> [Proxy a -> Laws] -> IO ()
typeLaws p ls = traverse_ lawsCheck $ fmap ($ p) ls

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

semiringLaws :: forall a. (TestableLaws a, Semiring a) => Proxy a -> Laws
semiringLaws p = mklaws "Semiring" p
    [ identity @a (+) zero
    , commutativity @a (+)
    , associativity @a (+)

    , associativity @a (*)
    , distributivity @a (*) (+)
    , identity @a (*) one

    , annihilation @a (*) zero
    ]

nearSemiringLaws :: forall a. (TestableLaws a, NearSemiring a) => Proxy a -> Laws
nearSemiringLaws p = mklaws "NearSemiring" p
    [ identity @a (+) zero
    , commutativity @a (+)
    , associativity @a (+)

    , associativity @a (*)
    , leftDistributivity @a (*) (+)

    , annihilation @a (*) zero
    ]

-- instance (Arbitrary a) => Arbitrary (V2 a) where
--     arbitrary = do
--         x <- arbitrary
--         y <- arbitrary
--         pure $ V2 x y

