module Algebra.Monoid
    ( Monoid(..)
    , CommutativeMonoid(..)
    ) where
--------------------------------------------------------------------------------
import           Prelude            hiding (Semigroup(..), Monoid(..), (+), (*))
import qualified Prelude
import           Data.Monoid        (Sum(..), Product(..), Any(..), All(..))
import           Algebra.Semigroup
--------------------------------------------------------------------------------

{-|
a monoid is a type with an associative binary operation, and an identity
element.

@
    -- left and right identity
    a <> mempty === mempty <> a === a
@
-}
class (Semigroup a) => Monoid a where
    mempty  :: a

{-|
a 'Monoid', but commutative
-}
class (CommutativeSemigroup a, Monoid a) => CommutativeMonoid a

--------------------------------------------------------------------------------

instance Monoid Any where
    mempty = Any False

instance Monoid All where
    mempty = All True

instance Monoid [a] where
    mempty = []

instance Monoid (a -> a) where
    mempty = id

instance Monoid (Maybe a) where
    mempty = Nothing

instance Monoid (Sum Int)        where mempty = 0
instance Monoid (Sum Integer)    where mempty = 0
instance Monoid (Sum Double)     where mempty = 0
instance Monoid (Sum Float)      where mempty = 0

instance Monoid (Product Int)        where mempty = 1
instance Monoid (Product Integer)    where mempty = 1
instance Monoid (Product Double)     where mempty = 1
instance Monoid (Product Float)      where mempty = 1
instance Monoid (Product Rational)   where mempty = 1

instance CommutativeMonoid (Sum Int)
instance CommutativeMonoid (Sum Integer)
instance CommutativeMonoid (Sum Double)
instance CommutativeMonoid (Sum Float)

instance CommutativeMonoid (Product Int)
instance CommutativeMonoid (Product Integer)
instance CommutativeMonoid (Product Double)
instance CommutativeMonoid (Product Float)

