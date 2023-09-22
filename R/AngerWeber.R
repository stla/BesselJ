x <- 
  "re(k1*cos((x+i*y)*cos(t))*sin(t)^(2*(a+i*b)) + k2*cos((a+i*b)*t-(x+i*y)*sin(t)))"
y <- 
  "im(k1*cos((x+i*y)*cos(t))*sin(t)^(2*(a+i*b)) + k2*cos((a+i*b)*t-(x+i*y)*sin(t)))"

library(giacR)
giac <- Giac$new()

giac$execute(x)
# k1*cosh(y*cos(t))*cos(x*cos(t))*(re(sin(t)^(a+i*b))^2-im(sin(t)^(a+i*b))^2)+k1*sinh(y*cos(t))*sin(x*cos(t))*(+2*re(sin(t)^(a+i*b))*im(sin(t)^(a+i*b))) + k2*cosh(b*t-y*sin(t))*cos(a*t-x*sin(t))
# k1*cosh(y*cos(t))*cos(x*cos(t))*(re(sin(t)^(a+i*b))^2-im(sin(t)^(a+i*b))^2)+k1*sinh(y*cos(t))*sin(x*cos(t))*(+2*re(sin(t)^(a+i*b))*im(sin(t)^(a+i*b))) + k2*cosh(b*t-y*sin(t))*cos(a*t-x*sin(t))

giac$execute(y)
#

