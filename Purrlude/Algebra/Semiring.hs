{-# LANGUAGE DefaultSignatures #-}
module Algebra.Semiring
    ( Semiring(..)
    , NearSemiring(..)
    , CommutativeSemiring(..)
    ) where
--------------------------------------------------------------------------------
import           Prelude                hiding (Semigroup(..), Monoid(..), (+), (*))
import qualified Prelude
import           Algebra.Monoid
import           Control.Applicative    (liftA2)
import           Numeric.Natural
--------------------------------------------------------------------------------

{-|
a 'Semiring' without the requirement of a multiplicative identity, nor right
distributivity. Gives rise to a 'CommutativeMonoid' (@a@, @(+)@, @zero@) via
'Sum', and a 'Semigroup' (@a@, @(*)@) via 'Product'

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
    -- multiplicative annihilator
    a * zero === zero * a === zero
@
-}
class NearSemiring a where
    (+)     :: a -> a -> a
    zero    :: a

    (*)     :: a -> a -> a

    default (+)  :: (Num a) => a -> a -> a
    default zero :: (Num a) => a
    default (*)  :: (Num a) => a -> a -> a

    (+) = (Prelude.+)
    zero = 0
    (*) = (Prelude.*)

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
class (NearSemiring a) => Semiring a where
    one :: a
    default one  :: (Num a) => a
    one = 1

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

-- trivial; zero-ring
instance NearSemiring () where
    _ + _ = ()
    _ * _ = ()
    zero = ()

instance Semiring () where
    one = ()

--------------------------------------------------------------------------------
instance NearSemiring (Maybe a) where
    a + Nothing = a
    Nothing + b = b

    zero = Nothing

    Nothing * _     = Nothing
    _ * Nothing     = Nothing
    Just a * Just b = Just a
--------------------------------------------------------------------------------
instance NearSemiring Bool where
    (+) = (||)
    (*) = (&&)
    zero = False

instance Semiring Bool where one = True
instance CommutativeSemiring Bool

--------------------------------------------------------------------------------

instance NearSemiring Int where
    (+) = (Prelude.+)
    (*) = (Prelude.*)
    zero = 0

instance NearSemiring Integer where
    (+) = (Prelude.+)
    (*) = (Prelude.*)
    zero = 0

instance NearSemiring Double where
    (+) = (Prelude.+)
    (*) = (Prelude.*)
    zero = 0

instance NearSemiring Float where
    (+) = (Prelude.+)
    (*) = (Prelude.*)
    zero = 0

instance NearSemiring Rational where
    (+) = (Prelude.+)
    (*) = (Prelude.*)
    zero = 0

instance Semiring Int where one = 1
instance Semiring Integer where one = 1
instance Semiring Double where one = 1
instance Semiring Float where one = 1
instance Semiring Rational where one = 1
--------------------------------------------------------------------------------

instance CommutativeSemiring Int
instance CommutativeSemiring Integer
instance CommutativeSemiring Double
instance CommutativeSemiring Float
instance CommutativeSemiring Rational

