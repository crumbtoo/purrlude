module Algebra.Monoid
    ( Monoid(..)
    , CommutativeMonoid(..)
    , Semigroup(..)
    , CommutativeSemigroup(..)
    ) where
--------------------------------------------------------------------------------
import           Prelude            hiding (Semigroup(..), Monoid(..), (+), (*))
import qualified Prelude
import           Data.Monoid        (Sum(..), Product(..), Any(..), All(..))
import           Control.Applicative (liftA2)
import           Algebra.Semiring
--------------------------------------------------------------------------------

{-|
a Semigroup is a type with an associative binary operation. the following laws
hold for all @a,b,c :: a@

@
    a <> (b <> c) === (a <> b) <> c
@
-}
class Semigroup a where
    (<>) :: a -> a -> a

{-|
a 'Semigroup', but commutative
@
    a <> b === b <> a
@
-}
class (Semigroup a) => CommutativeSemigroup a

instance Semigroup (a -> a) where
    (<>) = (.)

instance Semigroup Any where
    -- logical conjunction
    Any a <> Any b = Any (a || b)

instance Semigroup All where
    -- logical disjunction
    All a <> All b = All (a && b)

instance Semigroup [a] where
    (<>) = (++)

instance Semigroup (Maybe a) where
    Nothing <> a = a
    Just a  <> _ = Just a

instance Semigroup () where
    () <> () = ()

--------------------------------------------------------------------------------

instance Semigroup (Sum Int)        where (<>) = liftA2 (+)
instance Semigroup (Sum Integer)    where (<>) = liftA2 (+)
instance Semigroup (Sum Double)     where (<>) = liftA2 (+)
instance Semigroup (Sum Float)      where (<>) = liftA2 (+)

instance Semigroup (Product Int)        where (<>) = liftA2 (*)
instance Semigroup (Product Integer)    where (<>) = liftA2 (*)
instance Semigroup (Product Double)     where (<>) = liftA2 (*)
instance Semigroup (Product Float)      where (<>) = liftA2 (*)
instance Semigroup (Product Rational)   where (<>) = liftA2 (*)

instance CommutativeSemigroup (Sum Int)
instance CommutativeSemigroup (Sum Integer)
instance CommutativeSemigroup (Sum Double)
instance CommutativeSemigroup (Sum Float)

instance CommutativeSemigroup (Product Int)
instance CommutativeSemigroup (Product Integer)
instance CommutativeSemigroup (Product Double)
instance CommutativeSemigroup (Product Float)


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

@
    -- commutativity
    a <> b === b <> a
@
-}
class (CommutativeSemigroup a, Monoid a) => CommutativeMonoid a

--------------------------------------------------------------------------------

instance Monoid Any where
    mempty = Any False

instance Monoid All where
    mempty = All True

--------------------------------------------------------------------------------

instance Monoid [a] where
    mempty = []

instance Monoid (a -> a) where
    mempty = id

instance Monoid (Maybe a) where
    mempty = Nothing

instance Monoid () where
    mempty = ()

--------------------------------------------------------------------------------

instance Monoid (Sum Int)        where mempty = Sum 0
instance Monoid (Sum Integer)    where mempty = Sum 0
instance Monoid (Sum Double)     where mempty = Sum 0
instance Monoid (Sum Float)      where mempty = Sum 0

instance Monoid (Product Int)        where mempty = Product 1
instance Monoid (Product Integer)    where mempty = Product 1
instance Monoid (Product Double)     where mempty = Product 1
instance Monoid (Product Float)      where mempty = Product 1
instance Monoid (Product Rational)   where mempty = Product 1

instance CommutativeMonoid (Sum Int)
instance CommutativeMonoid (Sum Integer)
instance CommutativeMonoid (Sum Double)
instance CommutativeMonoid (Sum Float)

instance CommutativeMonoid (Product Int)
instance CommutativeMonoid (Product Integer)
instance CommutativeMonoid (Product Double)
instance CommutativeMonoid (Product Float)

