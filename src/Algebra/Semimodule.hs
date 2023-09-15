{-# LANGUAGE DefaultSignatures, FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies, TypeOperators #-}
module Algebra.Semimodule
    ( Semimodule(..)
    ) where
--------------------------------------------------------------------------------
import           Prelude
import           Algebra.Semiring
--------------------------------------------------------------------------------

{- consider:
 -   class (NearSemiring r) => NearSemimodule r m | m -> r
 - we can maybe try this if i think of any funny instances -}

{-|
a left module over a semiring. @r@ is a scalar type for @m@

instances should respect the following laws, for all @r, s :: r@ and 
@
    -- todo
@
-}
class (Semiring r) => Semimodule r m | m -> r where
    (!*) :: r -> m -> m

    default (!*) :: (r ~ m) => r -> m -> m
    (!*) = (*)

--------------------------------------------------------------------------------

instance Semimodule Int Int
instance Semimodule Integer Integer
instance Semimodule Float Float
instance Semimodule Double Double
instance Semimodule Rational Rational

