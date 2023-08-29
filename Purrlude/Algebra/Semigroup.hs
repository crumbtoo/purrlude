module Algebra.Semigroup
    ( Semigroup(..)
    , CommutativeSemigroup

    -- newtypes
    , Sum(..)
    , Product(..)
    , Any(..)
    , All(..)
    )
    where
--------------------------------------------------------------------------------
import           Prelude                hiding (Semigroup(..))
import qualified Prelude
import           Data.Monoid            (Sum(..), Product(..), Any(..), All(..))
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

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

instance Semigroup (Sum Int)        where (<>) = (+)
instance Semigroup (Sum Integer)    where (<>) = (+)
instance Semigroup (Sum Double)     where (<>) = (+)
instance Semigroup (Sum Float)      where (<>) = (+)

instance Semigroup (Product Int)        where (<>) = (*)
instance Semigroup (Product Integer)    where (<>) = (*)
instance Semigroup (Product Double)     where (<>) = (*)
instance Semigroup (Product Float)      where (<>) = (*)
instance Semigroup (Product Rational)   where (<>) = (*)

instance CommutativeSemigroup (Sum Int)
instance CommutativeSemigroup (Sum Integer)
instance CommutativeSemigroup (Sum Double)
instance CommutativeSemigroup (Sum Float)

instance CommutativeSemigroup (Product Int)
instance CommutativeSemigroup (Product Integer)
instance CommutativeSemigroup (Product Double)
instance CommutativeSemigroup (Product Float)

