{-# LANGUAGE TypeFamilies, DefaultSignatures, TypeOperators #-}
module Algebra.Scalable
    ( Scalable(..)
    , scale
    ) where
--------------------------------------------------------------------------------
import qualified Prelude            as Pre
import           Algebra.Semiring
--------------------------------------------------------------------------------

{-|
scaling generalises multiplication from @factor * factor@ to @scalar * factor@.
when @v@ and @s@ are of the same type, and instances of @Num@,
@(!*) = (Prelude.*)@
-}
class Scalable v where
    type Scalar v :: *

    -- | left scalar multiplication
    (!*) :: Scalar v -> v        -> v
    -- | right scalar multiplication
    (*!) :: v        -> Scalar v -> v

    type Scalar v = v
    default (!*) :: (Scalar v ~ v, Semiring v) => Scalar v -> v -> v
    default (*!) :: (Scalar v ~ v, Semiring v) => v -> Scalar v -> v
    (!*) = (*)
    (*!) = (*)

{- consider, instead of Scalable:
 -  class Mult a b r | a b -> r where
 -      (*) :: a -> b -> r
 -
 - my problem with this is that the operators are ambigious when partially
 - applied, however this does allow you to use (*) instead of (!*) and (*!), as
 - well as using the same operator for internal multiplication (a -> a -> a). -}

-- | prefix synonym for @(!*)@
scale :: (Scalable v) => Scalar v -> v -> v
scale = (!*)

