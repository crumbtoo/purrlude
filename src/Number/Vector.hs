{-# LANGUAGE TypeFamilies #-}
module Number.Vector where
--------------------------------------------------------------------------------
import           Prelude                (Show, Eq)
import           Data.Functor
import           Control.Applicative
import           Control.Monad
import           Algebra.Scalable
import           Algebra.Semiring
--------------------------------------------------------------------------------

data V2 a = V2 a a
    deriving (Show, Eq)

instance (NearSemiring a) => Scalable (V2 a) where
    type Scalar (V2 a) = a
    s !* (V2 a b) = V2 (s*a) (s*b)

--------------------------------------------------------------------------------

instance Functor V2 where
    fmap f (V2 a b) = V2 (f a) (f b)

instance Applicative V2 where
    pure a = V2 a a

    -- V2 f g <*> V2 a b = V2 (f a) (g b)
    liftA2 f (V2 a b) (V2 c d) = V2 (f a c) (f b d)

