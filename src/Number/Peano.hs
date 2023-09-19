{-# LANGUAGE GADTs, PatternSynonyms #-}
module Number.Peano
    ( Peano(..)
    , (^)
    )
    where
--------------------------------------------------------------------------------
import           Prelude            hiding (Semigroup(..), Monoid(..), (+), (*))
import           Algebra.Semiring
--------------------------------------------------------------------------------

data Peano where
    Zero :: Peano
    Succ :: Peano -> Peano

instance NearSemiring Peano where
    a + Zero = a
    a * (Succ b) = a * b + a

    zero = Zero

instance Semiring Peano where
    one = Succ Zero

instance CommutativeNearSemiring Peano
instance CommutativeSemiring Peano

(^) :: Peano -> Peano -> Peano
a ^ Zero = one
a ^ (Succ b) = a^b * a

