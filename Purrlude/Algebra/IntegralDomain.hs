module Purrlude.Algebra.IntegralDomain
    ( IntegralDomain(..)
    )
    where
--------------------------------------------------------------------------------
import           Prelude            hiding (div, mod, divMod)
import qualified Prelude
import           Purrlude.Algebra.Ring
--------------------------------------------------------------------------------
{-|
an 'IntegralDomain'
-}
class CommutativeRing a => IntegralDomain a where
    div    :: a -> a -> a
    mod    :: a -> a -> a
    divMod :: a -> a -> (a, a)

    div a b = fst $ divMod a b
    mod a b = snd $ divMod a b

    divMod a b = (a `div` b, a `mod` b)

    {-# MINIMAL div, mod | divMod #-}

