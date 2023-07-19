{-# LANGUAGE GADTs, PatternSynonyms #-}
module Algebra.Ring
    ( Ring(..)
    ) where
--------------------------------------------------------------------------------
import           Prelude            hiding (Semigroup(..), Monoid(..), (*), (+)
                                           , negate, (-))
import qualified Prelude            as Prelude
import           Algebra.Semiring
--------------------------------------------------------------------------------

{-
a Ring is a Semiring with negation, and thereby subtraction. a Ring is an
abelian group under addition, and a monoid under multiplication (see Sum and
Product newtypes). Abides by the Semiring laws, in addition to the following:

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

--------------------------------------------------------------------------------

instance Ring Int where
    negate = Prelude.negate
    (-) = (Prelude.-)

    {-# INLINE negate #-}
    {-# INLINE (-) #-}

instance Ring Integer where
    negate = Prelude.negate
    (-) = (Prelude.-)

    {-# INLINE negate #-}
    {-# INLINE (-) #-}

instance Ring Double where
    negate = Prelude.negate
    (-) = (Prelude.-)

    {-# INLINE negate #-}
    {-# INLINE (-) #-}

