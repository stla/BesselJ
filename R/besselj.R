# Re(t^z)
realPartTpowz <- function(t, z) {
  x <- Re(z)
  y <- Im(z)
  t^x * cos(y * log(t^2))
} 

# Im(t^z)
imagPartTpowz <- function(t, z) {
  x <- Re(z)
  y <- Im(z)
  t^x * sin(y * log(t))
} 

# Re(cos(z * cos(t)))
ReCosZcosT <- function(t, z) {
  x <- Re(z)
  y <- Im(z)
  cos(x*cos(t))*cosh(y*cos(t))
}

# Im(cos(z * cos(t)))
ImCosZcosT <- function(t, z) {
  x <- Re(z)
  y <- Im(z)
  -sin(x*cos(t))*sinh(y*cos(t))
}

z <- 2 
nu <- 1
t <- 1.5
# 
ReCosZcosT(t, z) * realPartTpowz(sin(t), 2*nu) - 
  ImCosZcosT(t, z) * imagPartTpowz(sin(t), 2*nu)

Re(cos(z*cos(t)) * (sin(t))^(2*nu))

# Exp[-(x+I*y)*sinh(t/(1-t)) - nu * t/(1-t)] * 1/(1-t)^2

# "real(exp(-(x+i*y)*sinh(t/(1-t)) - (x1+i*y1) * t/(1-t)) * 1/(1-t)^2"

# > giac$execute(e)
# [1] "exp(-x*sinh(t/(1-t))-x1*t/(1-t))*cos(-y*sinh(t/(1-t))-y1*t/(1-t))/(1-t)^2"
# im 
# [1] "exp(-x*sinh(t/(1-t))-x1*t/(1-t))*sin(-y*sinh(t/(1-t))-y1*t/(1-t))/(1-t)^2"

x = 2
y = 3
x1 = 4
y1 = 2

fr <- function(t) {
  exp(-x*sinh(t/(1-t))-x1*t/(1-t))*cos(-y*sinh(t/(1-t))-y1*t/(1-t))/(1-t)^2
}

integrate(fr, 0, .999)


# "re(exp(-(x+i*y)*sinh(t) - (x1+i*y1) * t)"
