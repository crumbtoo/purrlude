{-# LANGUAGE GADTs, PatternSynonyms #-}
module Number.Peano
    -- ( Peano(..)
    -- , infinity
    -- )
    where
{-
--------------------------------------------------------------------------------
import           Prelude            hiding (Semigroup(..), Monoid(..), (+), (*))
import           Algebra.Semiring
--------------------------------------------------------------------------------

-- | lazy natural numbers, zero to infinity
data Peano  ::  * where
     Zero   ::  Peano
     Succ   ::  Peano -> Peano

pattern S n = Succ n

infinity :: Peano
infinity = S infinity

deriving instance Eq Peano

instance Semiring Peano where
    Zero     + n        = n
    n        + Zero     = n
    (Succ a) + b        = Succ (a + b)

instance Show Peano where
    show = show . toInteger

instance Integral Peano where
    toInteger (Zero) = 0
    toInteger (S n)  = 1 + toInteger n

instance Enum Peano where
    -- TODO
instance Real Peano where
    -- TODO
instance Ord Peano where
    -- TODO
instance Num Peano where
    -- TODO

-}
