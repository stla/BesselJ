module Main where
import           Approx           ( assertAreClose )
import           Data.Complex     ( Complex(..), conjugate )
import           Test.Tasty       ( defaultMain, testGroup )
import           Test.Tasty.HUnit ( testCase )
import           Math.BesselJ     ( BesselResult(..), besselJ )
import           Math.AngerJ      ( AngerResult(..), angerJ )

i_ :: Complex Double
i_ = 0.0 :+ 1.0

aResult :: AngerResult -> Complex Double
aResult (AngerResult r  _  _) = r

bResult :: BesselResult -> Complex Double
bResult (BesselResult r _ _) = r

main :: IO ()
main = defaultMain $
  testGroup "Tests"
  [ 
    testCase "nu = 1+2i -- z = 3+4i" $ do
      my <- bResult <$> besselJ (1 :+ 2) (3 :+ 4) 1e-5 5000
      let wolfram = 0.31925 :+ (-0.66956) 
      assertAreClose "" 1e-5 my wolfram,

    -- testCase "Relation Bessel-J" $ do
    --   let nu = 0.5 :+ 2
    --       z = 3 :+ 4
    --       y = 2 * sin (nu * pi) / (pi * z)
    --   x1 <- bResult <$> jnu (nu-1) z 
    --   x2 <- bResult <$> jnu (-nu) z
    --   x3 <- bResult <$> jnu (1-nu) z
    --   x4 <- bResult <$> jnu nu z 
    --   assertAreClose "" 1e-3 (x1*x2 + x3*x4) y

    testCase "recurrence relation" $ do
      let nu = 0.5 :+ 2
          z = 3 :+ 4
      x1 <- bResult <$> besselJ nu z 1e-5 5000
      x2 <- bResult <$> besselJ (nu+1) z 1e-5 5000
      x3 <- bResult <$> besselJ (nu+2) z 1e-5 5000
      let y = 2*(nu+1)/z * x2 - x3
      assertAreClose "" 1e-7 x1 y,

    testCase "elementary equality" $ do
      let z = 3 :+ 4
          s = sqrt(2 / pi / z) * sin z
      x <- bResult <$> besselJ 0.5 z 1e-5 5000
      assertAreClose "" 1e-9 x s,

    testCase "remove square root" $ do
      let z = 2 :+ 1
          nu = (-0.3) :+ 1
      x <- bResult <$> besselJ nu (sqrt (z*z)) 1e-5 5000
      y <- bResult <$> besselJ nu z 1e-5 5000
      assertAreClose "" 1e-6 x (z**(-nu) * (z*z)**(nu/2) * y),

    testCase "remove square root --- integer nu" $ do
      let z = 2 :+ 1
          nu = -4
      x <- bResult <$> besselJ nu (sqrt (z*z)) 1e-5 5000
      y <- bResult <$> besselJ nu z 1e-5 5000
      assertAreClose "" 1e-6 x (z**(-nu) * (z*z)**(nu/2) * y),

    testCase "remove minus sign" $ do
      let z = 2 :+ 1
          nu = (-0.3) :+ 1
      x <- bResult <$> besselJ nu (-z) 1e-5 5000
      y <- bResult <$> besselJ nu z 1e-5 5000
      assertAreClose "" 1e-6 x ((-z)**nu * z**(-nu) * y), 

    testCase "conjugate" $ do
      let z = 2 :+ 5
          nu = 0.3 :+ (-1)
      x <- bResult <$> besselJ (conjugate nu) (conjugate z) 1e-5 5000
      y <- bResult <$> besselJ nu z 1e-5 5000
      assertAreClose "" 1e-6 x (conjugate y), 

    testCase "conjugate --- integer nu" $ do
      let z = 2 :+ 5
          nu = 7
      x <- bResult <$> besselJ nu (conjugate z) 1e-5 5000
      y <- bResult <$> besselJ nu z 1e-5 5000
      assertAreClose "" 1e-6 x (conjugate y),


    -- Anger --------

    testCase "Anger at z = 0" $ do
      let nu = (-2.3) :+ 1
          y = sin(pi*nu) / (pi*nu)
      x <- aResult <$> angerJ nu 0 1e-5 5000
      assertAreClose "" 1e-3 x y,

    testCase "Anger - remove minus sign" $ do
      let z = 2 :+ 1
          nu = (-0.3) :+ 1
      x <- aResult <$> angerJ nu (-z) 1e-5 5000
      y <- aResult <$> angerJ (-nu) z 1e-5 5000
      assertAreClose "" 1e-6 x y,

    testCase "Anger - recurrence relation" $ do
      let z = (-2) :+ 1
          nu = (-0.3) :+ 1
      x1 <- aResult <$> angerJ (nu-1) z 1e-5 5000
      x2 <- aResult <$> angerJ (nu+1) z 1e-5 5000
      y <- aResult <$> angerJ nu z 1e-5 5000
      let x = x1 + x2 
          y' = 2*nu/z * y - 2*sin(pi*nu)/(pi*z)
      assertAreClose "" 1e-6 x y'

  ]
