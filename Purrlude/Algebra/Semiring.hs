{-# LANGUAGE DefaultSignatures #-}
module Purrlude.Algebra.Semiring
    ( Semiring(..)
    , CommutativeSemiring(..)
    ) where
--------------------------------------------------------------------------------
import           Prelude                hiding (Semigroup(..), Monoid(..), (+), (*))
import qualified Prelude
import           Purrlude.Algebra.Monoid
import           Control.Applicative    (liftA2)
import           Numeric.Natural
--------------------------------------------------------------------------------

{-|
a 'Semiring' is a structure for which addition and multiplication are defined
and abide by some familiar laws. the looser requirements than a 'Ring' or
'Field' permit lawful instances for types such as @Nat@. every 'Semiring' gives
rise to both a 'CommutativeMonoid' (addition and zero) and a 'Monoid'
(multiplication and one), via 'Sum' and 'Product', rspectively.

@
    -- commutivity of addition
    a + b === b + a
    -- associativity of addition
    a + (b + c) === (a + b) + c
    -- additive identity
    a + zero === a

    -- associativity of multiplication
    a * (b * c) === (a * b) * c
    -- distributivity of multiplication
    a * (b + c) === (a * b) + (a * c)
    (b + c) * a === (b * a) + (c * a)
    -- multiplicative identity
    a * one === one * a === a
    -- multiplicative annihilator
    a * zero === zero * a === zero
@
-}
class (Eq a) => Semiring a where
    (+)     :: a -> a -> a
    zero    :: a

    (*)     :: a -> a -> a
    one     :: a

    default (+)  :: (Num a) => a -> a -> a
    default zero :: (Num a) => a
    default (*)  :: (Num a) => a -> a -> a
    default one  :: (Num a) => a

    (+) = (Prelude.+)
    zero = 0
    (*) = (Prelude.*)
    one = 1

    {-# MINIMAL (+), zero, (*), one #-}

infix 7 *
infix 6 +

{-|
a 'Semiring', but multiplication is commutative

@
    a * b === b * a
@
-}
class (Semiring a) => CommutativeSemiring a

--------------------------------------------------------------------------------

instance Semiring () where
    _ + _ = ()
    _ * _ = ()
    one = ()
    zero = ()

--------------------------------------------------------------------------------

instance Semiring Int where
    (+) = (Prelude.+)
    (*) = (Prelude.*)
    one = 1
    zero = 0

instance Semiring Integer where
    (+) = (Prelude.+)
    (*) = (Prelude.*)
    one = 1
    zero = 0

instance Semiring Double where
    (+) = (Prelude.+)
    (*) = (Prelude.*)
    one = 1
    zero = 0

instance Semiring Float where
    (+) = (Prelude.+)
    (*) = (Prelude.*)
    one = 1
    zero = 0

instance Semiring Bool where
    (+) = (||)
    zero = False
    (*) = (&&)
    one = True

instance Semiring Rational where
    (+) = (Prelude.+)
    (*) = (Prelude.*)
    one = 1
    zero = 0

--------------------------------------------------------------------------------

instance CommutativeSemiring Int
instance CommutativeSemiring Integer
instance CommutativeSemiring Double
instance CommutativeSemiring Float
instance CommutativeSemiring Bool
instance CommutativeSemiring Rational

