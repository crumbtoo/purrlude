{-# LANGUAGE TemplateHaskell, RankNTypes, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
module Main (main) where
--------------------------------------------------------------------------------
import           Prelude                            hiding ( Semigroup, (<>)
                                                           , Monoid, mempty
                                                           , (+), (*)
                                                           )
import           Control.Monad
import           Data.Proxy
import           Test.QuickCheck
import           Test.QuickCheck.Classes            ( Laws(..), lawsCheck
                                                    , lawsCheckMany
                                                    )
import           Text.Printf                        (printf)
import           Data.Foldable                      (traverse_)

import           Algebra.Semiring
import           Algebra.Monoid
import           Number.Vector

import           TypeLaws
--------------------------------------------------------------------------------

main :: IO ()
main = do
    test_Sum_Int
    test_Product_Int
    -- test_Maybe_Int
    test_V2_Int
    test_Bool
    test_unit

