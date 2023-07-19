module Algebra.Monoid
    ( Monoid(..)
    ) where
--------------------------------------------------------------------------------
import           Prelude            hiding (Semigroup(..), Monoid(..), (+), (*))
import qualified Prelude
import           Data.Monoid        (Sum(..), Product(..))
--------------------------------------------------------------------------------

{-|
a monoid is a type with an associative binary operation and an identity
element.

@
    -- associativity
    a <> (b <> c) == (a <> b) <> c
    -- left and right identity
    a <> mempty == mempty <> a == a
@
-}
class Monoid a where
    (<>)    :: a -> a -> a
    mempty  :: a

--------------------------------------------------------------------------------

