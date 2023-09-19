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
a left module over a semiring. @r@ is a scalar type for @m@

instances should respect the following laws, for all @r, s :: r@ and 
@
    -- todo
@
-}

class Semimodule m where
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

