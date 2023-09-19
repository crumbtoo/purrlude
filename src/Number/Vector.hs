{-# LANGUAGE TypeFamilies #-}
module Number.Vector
    ( V2(V2)
    )
    where
--------------------------------------------------------------------------------
import           Prelude
import           Data.Functor
import           Control.Applicative
import           Control.Monad
import           Algebra.Semiring
import           Algebra.Semimodule
import           Algebra.Monoid
--------------------------------------------------------------------------------

data V2 a = V2 a a
    deriving (Show, Eq)

instance (NearSemiring a) => NearSemiring (V2 a) where
    (V2 a b) + (V2 c d) = V2 (a + c) (b + d)
    -- | Hadamard product / element-wise product / naive product
    (V2 a b) * (V2 c d) = V2 (a * c) (b * d)
    zero = V2 zero zero

instance (Semiring a) => Semiring (V2 a) where
    one = V2 one one

instance (CommutativeNearSemiring a) => CommutativeNearSemiring (V2 a)
instance (CommutativeSemiring a) => CommutativeSemiring (V2 a)

instance (Semiring a) => Semimodule (V2 a) where
    type SemimoduleScalar (V2 a) = a
    r !* V2 a b = V2 (r*a) (r*b)

--------------------------------------------------------------------------------

instance Functor V2 where
    fmap f (V2 a b) = V2 (f a) (f b)

instance Applicative V2 where
    pure a = V2 a a
    liftA2 f (V2 a b) (V2 c d) = V2 (f a c) (f b d)

instance Monad V2 where
    m >>= k = join' (fmap k m)
        where
            -- the diagonal of a 2x2 matrix :D
            join' (V2 (V2 a _) (V2 _ d)) = V2 a d

