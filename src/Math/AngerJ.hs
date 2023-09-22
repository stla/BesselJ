module Math.AngerJ
  ( AngerResult(..), angerJ )
  where
import Data.Complex          ( imagPart, realPart, Complex(..) )
import Numerical.Integration ( integration, IntegralResult(..) )
import Foreign.C             ( CDouble )

-- | Data type to store the result of a computation of the Anger J-function.
-- The fields are @_result@ for the value, @_errors@ for the error estimates 
-- of the integrals used for the computation, and @_codes@ for the convergence 
-- codes of these integrals (0 for success).
data AngerResult = AngerResult {
    _result :: Complex Double
  , _errors :: (Double, Double)
  , _codes  :: (Int, Int)
} deriving Show

cpxdbl2cpxcdbl :: Complex Double -> Complex CDouble
cpxdbl2cpxcdbl z = realToFrac (realPart z) :+ realToFrac (imagPart z)


-- | Anger-J function. It is computed with two integrals. The field @_errors@ 
-- in the result are the error estimates of the integrals. The field @_codes@ 
-- provides the code indicating success (0) or failure of each integral.
angerJ :: Complex Double  -- ^ order, complex number 
       -> Complex Double  -- ^ the variable, a complex number
       -> Double          -- ^ target relative accuracy for the integrals, e.g. 1e-5
       -> Int             -- ^ number of subdivisions for the integrals, e.g. 5000
       -> IO AngerResult  -- ^ result
angerJ nu z err subdiv = do
  let z' = cpxdbl2cpxcdbl z
      x' = realPart z'
      y' = imagPart z'
      nu' = cpxdbl2cpxcdbl nu
      a' = realPart nu'
      b' = imagPart nu'
      reintegrand t = cosh(b' * t - y' * sin t) * cos(a' * t - x' * sin t)
      imintegrand t = -sinh(b' * t - y' * sin t) * sin(a' * t - x' * sin t)
  re <- integration reintegrand 0 pi 0.0 err subdiv
  im <- integration imintegrand 0 pi 0.0 err subdiv
  return AngerResult {
      _result = (_value re :+ _value im) / pi
    , _errors = (_error re, _error im)
    , _codes  = (_code re, _code im)
  }


