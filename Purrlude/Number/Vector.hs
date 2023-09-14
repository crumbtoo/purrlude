{-# LANGUAGE TypeFamilies #-}
module Number.Vector where
--------------------------------------------------------------------------------
import           Prelude                (Show, Eq)
import           Algebra.Scalable
import           Algebra.Semiring
--------------------------------------------------------------------------------

data V2 a = V2 a a
    deriving (Show, Eq)

instance (Semiring a) => Scalable (V2 a) where
    type Scalar (V2 a) = a
    s !* V2 a b = V2 (s*a) (s*b)
    V2 a b *! s = V2 (a*s) (b*s)

