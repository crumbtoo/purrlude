module Data.Free
    ( FreeMonoid(FreeMonoid)
    )
    where
--------------------------------------------------------------------------------
import           Algebra.Monoid
--------------------------------------------------------------------------------

-- | wraps a type adding an additional value defined to be an identity under
-- @(<>)@
data FreeMonoid a = FreeMonoid a
                  | FreeIdentity

instance (Semigroup a) => Semigroup (FreeMonoid a) where
    a <> FreeIdentity = a
    FreeIdentity <> b = b

    FreeMonoid a <> FreeMonoid b = FreeMonoid (a <> b)

instance (Semigroup a) => Semigroup (FreeMonoid a) where
    mempty = FreeIdentity

