module Algebra.Group
    ( Group(..)
    ) where
--------------------------------------------------------------------------------
import           Prelude            hiding ((+), negate, Monoid(..), Semigroup(..))
import           Algebra.Monoid
import           Data.Monoid        (Sum, Product)
--------------------------------------------------------------------------------

class (Monoid a) => Group a where
    inverse :: a -> a

