{-# LANGUAGE FunctionalDependencies #-}
module Number.Vector where
--------------------------------------------------------------------------------
import           Algebra.Scalable
--------------------------------------------------------------------------------

data V2 a = V2 a a
    deriving (Show, Eq)

-- instance (Scalable a Int) => Scalable (V2 a) Int where
--     scale s (V2 x y) = V2 (scale s x) (scale s y)

