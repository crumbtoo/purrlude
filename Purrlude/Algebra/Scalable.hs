{-# LANGUAGE FunctionalDependencies, DefaultSignatures #-}
module Purrlude.Algebra.Scalable
    ( Scalable(..)
    ) where
--------------------------------------------------------------------------------
import qualified Prelude            as Pre
--------------------------------------------------------------------------------

{-|
scaling generalises multiplication from @factor * factor@ to @scalar * factor@.
when @v@ and @s@ are of the same type, and instances of @Num@,
@scale = (Prelude.*)@
-}
class Scalable v s where
    scale :: s -> v -> v

