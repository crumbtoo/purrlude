module Algebra.Ring
    ( Ring(..)
    , CommutativeRing(..)
    ) where
--------------------------------------------------------------------------------
import           Prelude            hiding (Semigroup(..), Monoid(..), (*), (+)
                                           , negate, (-))
import qualified Prelude            as Prelude
import           Algebra.Semiring
--------------------------------------------------------------------------------

{-
a 'Ring' is a 'Semiring' with negation, and thereby subtraction. a 'Ring' gives
rise to a 'CommutativeGroup' (addition, negate, zero) and a 'Monoid'
(multiplication, one) via the 'Sum' and 'Product' wrappers, respectively. Abides
by the Semiring laws, in addition to the following:

@
    -- additive inverse
    a + (negate a) === zero

    -- subtraction-negation equivalence
    a - b === a + (negate b)
@
-}
class (Semiring a) => Ring a where
    negate :: a -> a
    (-)    :: a -> a -> a

    negate = (zero-)
    a - b  = a + negate b

    {-# MINIMAL negate | (-) #-}

{-|
a 'Ring', but multiplication is commutative
-}
class (Ring a) => CommutativeRing a

--------------------------------------------------------------------------------

instance Ring Int where
    negate = Prelude.negate
    (-) = (Prelude.-)

instance Ring Integer where
    negate = Prelude.negate
    (-) = (Prelude.-)

instance Ring Double where
    negate = Prelude.negate
    (-) = (Prelude.-)

instance Ring Float where
    negate = Prelude.negate
    (-) = (Prelude.-)

instance Ring Rational where
    negate = Prelude.negate
    (-) = (Prelude.-)

