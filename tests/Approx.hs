module Approx (assertAreClose) where
import           Data.Complex     ( magnitude, Complex(..) )
import           Test.Tasty.HUnit ( Assertion, assertBool )

areClose :: Double -> Complex Double -> Complex Double -> Bool
areClose epsilon z1 z2 = 
  let maxmod = if magnitude z2 < epsilon 
      then 1.0
      else max (magnitude z1) (magnitude z2)
  in 
    magnitude (z1 - z2) < 2.0 * epsilon * maxmod

-- assert approximate equality
assertAreClose :: 
  String -> Double -> Complex Double -> Complex Double -> Assertion
assertAreClose prefix epsilon z1 z2 = 
  assertBool prefix (areClose epsilon z1 z2)