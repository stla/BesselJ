library(giacR)
giac <- Giac$new()

x <- "re(cos((a+i*b)*t - (x+i*y)*sin(t)))"
y <- "im(cos((a+i*b)*t - (x+i*y)*sin(t)))"

giac$execute(x)
# cosh(b*t-y*sin(t))*cos(a*t-x*sin(t))

giac$execute(y)
# -sinh(b*t-y*sin(t))*sin(a*t-x*sin(t))