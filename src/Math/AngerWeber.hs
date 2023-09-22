module Math.AngerWeber
  ( AngerWeberResult(..), angerWeber )
  where
import Data.Complex          ( realPart, imagPart, Complex(..) )
import Math.BesselJ          ( besselJ, BesselResult(..) )
import Math.AngerJ           ( angerJ, AngerResult(..))


-- | Data type to store the result of a computation of the Anger-Weber function.
-- It is based on a computation of the Bessel J-function and a computation of 
-- the Anger J-function. 
-- The fields are @_result@ for the value, @_besselResult@ for the result of 
-- the computation of the Bessel J-function, and @_angerResult@ for the result 
-- of the computation of the Anger J-function.
data AngerWeberResult = AngerWeberResult {
    _result       :: Complex Double
  , _besselResult :: BesselResult
  , _angerResult  :: AngerResult
} deriving Show


isInteger :: Complex Double -> Bool
isInteger z = y == 0 && x == fromIntegral (floor x :: Int)
  where
    x = realPart z
    y = imagPart z


aResult :: AngerResult -> Complex Double
aResult (AngerResult r  _  _) = r

bResult :: BesselResult -> Complex Double
bResult (BesselResult r _ _) = r


-- | Anger-Weber function. 
angerWeber :: Complex Double       -- ^ order, non-integer complex number with real part larger than -0.5
           -> Complex Double       -- ^ the variable, a complex number
           -> Double               -- ^ target relative accuracy for the integrals, e.g. 1e-5
           -> Int                  -- ^ number of subdivisions for the integrals, e.g. 5000
           -> IO AngerWeberResult  -- ^ result
angerWeber nu z err subdiv 
  | isInteger nu = error "The order `nu` cannot be an integer."
  | realPart nu <= -0.5 = error "The real part of the order `nu` must be larger than -0.5."
  | otherwise = do
      bessel <- besselJ nu z err subdiv
      anger <- angerJ nu z err subdiv
      let bresult = bResult bessel
          aresult = aResult anger 
          result = (aresult - bresult) / sin(pi*nu)
      return (AngerWeberResult result bessel anger)
