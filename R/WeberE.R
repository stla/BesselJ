library(giacR)
giac <- Giac$new()

x <- "re(sin((a+i*b)*t - (x+i*y)*sin(t)))"
y <- "im(sin((a+i*b)*t - (x+i*y)*sin(t)))"

giac$execute(x)
# cosh(b*t-y*sin(t))*sin(a*t-x*sin(t))

giac$execute(y)
# -sinh(b*t-y*sin(t))*sin(a*t-x*sin(t))