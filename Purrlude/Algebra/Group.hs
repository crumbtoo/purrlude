module Algebra.Group
    ( Group(..)
    ) where
--------------------------------------------------------------------------------
import           Prelude            hiding ((+), Monoid(..), Semigroup(..))
import           Algebra.Monoid
import           Data.Monoid        (Sum(..), Product(..), Any(..), All(..))
import           Data.Ratio         ((%))
--------------------------------------------------------------------------------

{-|
a 'Group' is a type with an associative binary operation, an identity element
(see 'Monoid'), and every element has an inverse.

@
    -- left inverse
    inverse a <> a === mempty
    -- right inverse
    a <> inverse a === mempty
@
-}
class (Monoid a) => Group a where
    inverse :: a -> a

{-|
a 'Group' but commutative. often called an /Abelian Group/.

@
    a <> b === b <> a
@
-}
class (Group a) => CommutativeGroup a

--------------------------------------------------------------------------------


instance Group (Sum Int)        where inverse = negate
instance Group (Sum Integer)    where inverse = negate
instance Group (Sum Double)     where inverse = negate
instance Group (Sum Float)      where inverse = negate

instance Group (Product Double)     where inverse = fmap (1/)
instance Group (Product Float)      where inverse = fmap (1/)
instance Group (Product Rational)   where inverse = fmap (1/)

instance CommutativeGroup (Sum Int)
instance CommutativeGroup (Sum Integer)
instance CommutativeGroup (Sum Double)
instance CommutativeGroup (Sum Float)

instance CommutativeGroup (Product Double)
instance CommutativeGroup (Product Float)

