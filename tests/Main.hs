module Main where
import           Approx           ( assertAreClose )
import           Data.Complex     ( Complex(..), conjugate )
import           Test.Tasty       ( defaultMain, testGroup )
import           Test.Tasty.HUnit ( testCase )
import           Math.BesselJ     ( BesselResult(_result), besselJ )

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
      assertAreClose "" 1e-9 x s,

    testCase "remove square root" $ do
      let z = 2 :+ 1
          nu = (-0.3) :+ 1
      x <- _result <$> besselJ nu (sqrt (z*z)) 1e-5 5000
      y <- _result <$> besselJ nu z 1e-5 5000
      assertAreClose "" 1e-6 x (z**(-nu) * (z*z)**(nu/2) * y),

    testCase "remove square root --- integer nu" $ do
      let z = 2 :+ 1
          nu = -4
      x <- _result <$> besselJ nu (sqrt (z*z)) 1e-5 5000
      y <- _result <$> besselJ nu z 1e-5 5000
      assertAreClose "" 1e-6 x (z**(-nu) * (z*z)**(nu/2) * y),

    testCase "remove minus sign" $ do
      let z = 2 :+ 1
          nu = (-0.3) :+ 1
      x <- _result <$> besselJ nu (-z) 1e-5 5000
      y <- _result <$> besselJ nu z 1e-5 5000
      assertAreClose "" 1e-6 x ((-z)**nu * z**(-nu) * y), 

    testCase "conjugate" $ do
      let z = 2 :+ 5
          nu = 0.3 :+ (-1)
      x <- _result <$> besselJ (conjugate nu) (conjugate z) 1e-5 5000
      y <- _result <$> besselJ nu z 1e-5 5000
      assertAreClose "" 1e-6 x (conjugate y), 

    testCase "conjugate --- integer nu" $ do
      let z = 2 :+ 5
          nu = 7
      x <- _result <$> besselJ nu (conjugate z) 1e-5 5000
      y <- _result <$> besselJ nu z 1e-5 5000
      assertAreClose "" 1e-6 x (conjugate y)

  ]
