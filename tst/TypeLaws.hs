{-# LANGUAGE ConstraintKinds, RankNTypes #-}
module TypeLaws where
--------------------------------------------------------------------------------
import           ClassLaws
import           Data.Proxy
import           Data.Monoid        (Sum, Product)
import           Test.QuickCheck

import Number.Vector
--------------------------------------------------------------------------------

instance (Arbitrary a) => Arbitrary (V2 a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        pure $ V2 x y

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

test_Bool :: IO ()
test_Bool = typeLaws (Proxy :: Proxy Bool)
    [ semiringLaws
    ]

test_unit :: IO ()
test_unit = typeLaws (Proxy :: Proxy ())
    [ monoidLaws
    , semiringLaws
    ]

test_V2_Int :: IO ()
test_V2_Int = typeLaws (Proxy :: Proxy (V2 Int))
    [ commutativeSemiringLaws
    ]


