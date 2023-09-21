module Main where
import           Approx               ( assertAreClose )
import           Data.Complex         ( Complex(..) )
import           Math.BesselJ
-- import           Math.Gamma           ( gamma )
import           Test.Tasty           ( defaultMain, testGroup )
import           Test.Tasty.HUnit     ( testCase )

i_ :: Complex Double
i_ = 0.0 :+ 1.0


main :: IO ()
main = defaultMain $
  testGroup "Tests"
  [ 
    testCase "nu = 1+2i -- z = 3+4i" $ do
      my <- _result <$> besselJ (1 :+ 2) (3 :+ 4) 1e-5 5000
      let wolfram = 0.31925 :+ (-0.66956) 
      assertAreClose "" 1e-5 my wolfram,

    -- testCase "Relation Bessel-J" $ do
    --   let nu = 0.5 :+ 2
    --       z = 3 :+ 4
    --       y = 2 * sin (nu * pi) / (pi * z)
    --   x1 <- _result <$> jnu (nu-1) z 
    --   x2 <- _result <$> jnu (-nu) z
    --   x3 <- _result <$> jnu (1-nu) z
    --   x4 <- _result <$> jnu nu z 
    --   assertAreClose "" 1e-3 (x1*x2 + x3*x4) y

    testCase "recurrence relation" $ do
      let nu = 0.5 :+ 2
          z = 3 :+ 4
      x1 <- _result <$> besselJ nu z 1e-5 5000
      x2 <- _result <$> besselJ (nu+1) z 1e-5 5000
      x3 <- _result <$> besselJ (nu+2) z 1e-5 5000
      let y = 2*(nu+1)/z * x2 - x3
      assertAreClose "" 1e-7 x1 y,

    testCase "elementary equality" $ do
      let z = 3 :+ 4
          s = sqrt(2 / pi / z) * sin z
      x <- _result <$> besselJ 0.5 z 1e-5 5000
      assertAreClose "" 1e-9 x s

  ]
