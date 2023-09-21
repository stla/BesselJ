besselJnu' :: Complex Double
           -> Complex Double
           -> IO (Complex Double)
besselJnu' nu z = do
  --term1 <- jn n z 
  let z' = cpxdbl2cpxcdbl z
      x' = realPart z'
      y' = imagPart z'
      nu' = cpxdbl2cpxcdbl nu
      a' = realPart nu'
      b' = imagPart nu'
      reintegrand t = 
        exp(-x'*sinh(t/(1-t))-a'*t/(1-t)) 
          * cos(-y'*sinh(t/(1-t))-b'*t/(1-t)) / (1-t)**2
      imintegrand t = 
        exp(-x'*sinh(t/(1-t))-a'*t/(1-t)) 
          * sin(-y'*sinh(t/(1-t))-b'*t/(1-t)) / (1-t)**2
      cst = - sin(nu * pi) / pi
  re <- integration reintegrand 0 0.9 0.0 1e-5 500
  im <- integration imintegrand 0 0.9 0.0 1e-5 500
  return $ cst * (_value re :+ _value im)
