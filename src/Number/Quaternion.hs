module Number.Quaternion
    ( Quaternion
    , rot1
    , roti
    , rotj
    , rotk
    )
    where
--------------------------------------------------------------------------------
import           Prelude
import           Data.Complex       (Complex(..))
import           Algebra.Semiring
import           Algebra.Ring
--------------------------------------------------------------------------------

infix 5 :++
data Quaternion a = Complex a :++ Complex a
    deriving (Eq, Show)

-- instance (Ring a) => NearSemiring (Quaternion a) where
--     zero = zero :+ zero :++ zero :+ zero
--     (a :+ b :++ c :+ d) + (w :+ x :++ y :+ z)
--         = (a + w) :+ (b + x) :++ (c + y) :+ (d + z)

--     -- jesus.
--     (a :+ b :++ c :+ d) * (w :+ x :++ y :+ z)
--         = ( (a*w) - (b*x) - (c*y) - (d*z) )
--        :+ ( (a*x) + (b*w) + (c*z) - (d*y) )
--       :++ ( (a*y) - (b*y) + (c*w) + (d*x) )
--        :+ ( (a*z) + (b*y) - (c*x) + (d*w) )

rot1, roti, rotj, rotk :: Quaternion Int
rot1 = 1 :+ 0 :++ 0 :+ 0
roti = 0 :+ 1 :++ 0 :+ 0
rotj = 0 :+ 0 :++ 1 :+ 0
rotk = 0 :+ 0 :++ 0 :+ 1

