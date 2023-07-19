{-# LANGUAGE DefaultSignatures #-}
module Algebra.Semiring
    ( Semiring(..) )
    where
--------------------------------------------------------------------------------
import           Prelude                hiding (Semigroup(..), Monoid(..), (+), (*))
import qualified Prelude
import           Data.Monoid            (Sum(..), Product(..))
import           Algebra.Monoid
import           Control.Applicative    (liftA2)
import           Numeric.Natural
--------------------------------------------------------------------------------

{-|
a Semiring is a structure for which addition and multiplication are defined.
the looser requirements than a Ring or Field permit lawful instances for types
such as @Nat@.

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

--------------------------------------------------------------------------------

-- any Semiring gives rise to a Monoid, either under addition and zero, or
-- multiplication and one

instance (Semiring a) => Monoid (Sum a) where
    (<>) = liftA2 (+)
    mempty = Sum zero

    {-# INLINE (<>) #-}

instance (Semiring a) => Monoid (Product a) where
    (<>) = liftA2 (*)
    mempty = Product one

    {-# INLINE (<>) #-}

--------------------------------------------------------------------------------

instance Semiring Int where
    (+) = (Prelude.+)
    zero = 0

    (*) = (Prelude.*)
    one = 1

    {-# INLINE (+) #-}
    {-# INLINE (*) #-}

instance Semiring Integer where
    (+) = (Prelude.+)
    zero = 0

    (*) = (Prelude.*)
    one = 1

    {-# INLINE (+) #-}
    {-# INLINE (*) #-}

instance Semiring Natural where
    (+) = (Prelude.+)
    zero = 0

    (*) = (Prelude.*)
    one = 1

    {-# INLINE (+) #-}
    {-# INLINE (*) #-}

instance Semiring Double where
    (+) = (Prelude.+)
    zero = 0

    (*) = (Prelude.*)
    one = 1

    {-# INLINE (+) #-}
    {-# INLINE (*) #-}

