{-# LANGUAGE DefaultSignatures, FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies, TypeOperators #-}
module Algebra.Semimodule
    ( Semimodule(..)
    ) where
--------------------------------------------------------------------------------
import           Prelude
import           Data.Kind
import           Algebra.Semiring
import           Algebra.Group
import           Algebra.Monoid
--------------------------------------------------------------------------------

{- consider:
 -   class (NearSemiring r) => NearSemimodule r m | m -> r
 - we can maybe try this if i think of any funny instances; idk scalar
 - multiplication realy makes sense without an identity  -}

{-|
a left module over a semiring. defines scalar multiplication

instances should respect the following laws, for all @r, s :: SemimoduleScalar
m@ and @x, y :: m@
@
    -- left distributivity of scalar multiplication
    r !* (x <> y) === r !* x <> r !* y
    -- right distributivity of scalar multiplication
    (r + s) !* x === r !* x + s !* x
    -- associativity of scalar multiplication and non-scalar multiplication
    (r * s) !* x === r !* (s !* x)
    -- identity scalar
    one !* x === x
@
-}

class (NearSemiring m) => Semimodule m where
    type SemimoduleScalar m :: Type
    (!*) :: (Semiring (SemimoduleScalar m))
         => SemimoduleScalar m -> m -> m

-- potential class decl; haven't decided yet
-- class Semimodule m where
--     (!*) :: (Semiring r) => r -> m r -> m r

--------------------------------------------------------------------------------

-- instance Semimodule Int Int
-- instance Semimodule Integer Integer
-- instance Semimodule Float Float
-- instance Semimodule Double Double
-- instance Semimodule Rational Rational

