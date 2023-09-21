import Math.BesselJ
import Data.Complex

z :: Complex Double
z = 2 :+ 1

nu :: Complex Double
nu = 1.5 :+ 1

term1 :: IO (BesselJ)
term1 = jn (realPart nu) z

term2 :: IO (Complex Double)
term2 = besselJnu' nu z