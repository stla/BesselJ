module Math.BesselJ
  ( BesselResult(..), besselJ )
  where
import Data.Complex          ( imagPart, realPart, Complex(..) )
import Numerical.Integration ( integration, IntegralResult(..) )
import Math.Gamma            ( Gamma(gamma) )
import Foreign.C             ( CDouble )


-- | Data type to store the result of a computation of the Bessel J-function.
-- The fields are @_result@ for the value, @_errors@ for the error estimates 
-- of the integrals used for the computation, and @_codes@ for the convergence 
-- codes of these integrals (0 for success).
data BesselResult = BesselResult {
    _result :: Complex Double
  , _errors :: (Double, Double)
  , _codes  :: (Int, Int)
} deriving Show


cpxdbl2cpxcdbl :: Complex Double -> Complex CDouble
cpxdbl2cpxcdbl z = realToFrac (realPart z) :+ realToFrac (imagPart z)


dbl2cdbl :: Double -> CDouble
dbl2cdbl = realToFrac


-- | Bessel-J function. It is computed with two integrals. The field @_errors@ 
-- in the result are the error estimates of the integrals. The field @_codes@ 
-- is the code indicating success (0) or failure.
besselJn :: Int             -- ^ order
         -> Complex Double  -- ^ variable
         -> Double          -- ^ target relative error accuracy for the integrals
         -> Int             -- ^ number of subdivisions for the integrals
         -> IO BesselResult -- ^ result
besselJn n z err subdiv = do
  let z' = cpxdbl2cpxcdbl z
      n' = dbl2cdbl $ fromIntegral n
      a = realPart z'
      b = imagPart z'
  re <- integration 
    (\t -> (cos (a * sin t + n' * t) * cosh (b * sin t)) / pi) 
      0 pi 0.0 err subdiv
  im <- integration 
    (\t -> -(sin (a * sin t + n' * t) * sinh (b * sin t)) / pi) 
      0 pi 0.0 err subdiv
  return BesselResult {
      _result = _value re :+ _value im
    , _errors = (_error re, _error im)
    , _codes  = (_code re, _code im)
  }


-- Re(t^z)
realPartTpowz :: CDouble -> Complex CDouble -> CDouble
realPartTpowz t z =
  let x = realPart z
      y = imagPart z
  in
    t**x * cos (y * log t)

-- Im(t^z)
imagPartTpowz :: CDouble -> Complex CDouble -> CDouble
imagPartTpowz t z =
  let x = realPart z
      y = imagPart z
  in
    t**x * sin (y * log t)

-- Re(cos(z * cos(t)))
reCosZcosT :: CDouble -> Complex CDouble -> CDouble
reCosZcosT t z =
  let x = realPart z
      y = imagPart z
  in
    cos (x * cos t) * cosh (y * cos t)

-- Im(cos(z * cos(t)))
imCosZcosT :: CDouble -> Complex CDouble -> CDouble
imCosZcosT t z =
  let x = realPart z
      y = imagPart z
  in -sin (x * cos t) * sinh (y * cos t)


-- | Bessel-J function. It is computed with two integrals. The field @_errors@ 
-- in the result provides the error estimates of the integrals. The field 
-- @_codes@ provides the codes indicating success (0) or failure of each integral.
besselJnu :: Complex Double  -- ^ order, complex number with real part > -0.5
          -> Complex Double  -- ^ the variable, a complex number
          -> Double          -- ^ target relative accuracy for the integrals, e.g. 1e-5
          -> Int             -- ^ number of subdivisions for the integrals, e.g. 5000
          -> IO BesselResult -- ^ result
besselJnu nu z err subdiv = do
  let z' = cpxdbl2cpxcdbl z
      nu' = cpxdbl2cpxcdbl nu
      reintegrand t = reCosZcosT t z' * realPartTpowz (sin t) (2*nu')
        - imCosZcosT t z' * imagPartTpowz (sin t) (2*nu')
      imintegrand t = reCosZcosT t z' * imagPartTpowz (sin t) (2*nu')
        + imCosZcosT t z' * realPartTpowz (sin t) (2*nu')
      cst = (z/2)**nu / (sqrt pi * gamma (nu + 0.5))
  re <- integration reintegrand 0 pi 0.0 err subdiv
  im <- integration imintegrand 0 pi 0.0 err subdiv
  return BesselResult {
      _result = cst * (_value re :+ _value im)
    , _errors = (_error re, _error im)
    , _codes  = (_code re, _code im)
  }


isInteger :: Complex Double -> Bool
isInteger z = y == 0 && x == fromIntegral (floor x :: Int)
  where
    x = realPart z
    y = imagPart z

asInteger :: Complex Double -> Int
asInteger z = floor (realPart z) :: Int

-- | Bessel-J function. It is computed with two integrals. The field @_errors@ 
-- in the result provides the error estimates of the integrals. The field 
-- @_codes@ provides the codes indicating success (0) or failure of each integral.
besselJ :: Complex Double  -- ^ order, integer or complex number with real part > -0.5
        -> Complex Double  -- ^ the variable, a complex number
        -> Double          -- ^ target relative accuracy for the integrals, e.g. 1e-5
        -> Int             -- ^ number of subdivisions for the integrals, e.g. 5000
        -> IO BesselResult -- ^ result
besselJ nu z err subdiv
  | isInteger nu = besselJn (asInteger nu) z err subdiv
  | realPart nu > -0.5 = besselJnu nu z err subdiv
  | otherwise = error "Invalid value of the order."
