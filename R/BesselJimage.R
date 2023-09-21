library(RcppColors)
library(Bessel)
x <- y <- seq(-6, 6, len = 2048)
# complex grid
W <- outer(y, x, function(x, y) complex(real = x, imaginary = y))
# computes Bessel values
Z <- matrix(BesselJ(W, nu = 3), nrow = nrow(W), ncol = ncol(W))
# maps them to colors
image <- colorMap1(Z)
# plot
opar <- par(mar = c(0,0,0,0), bg = "#15191E")
plot(
  c(-100, 100), c(-100, 100), type = "n", 
  xlab = "", ylab = "", axes = FALSE, asp = 1
)
rasterImage(image, -100, -100, 100, 100)
par(opar)

svg("x.svg", width = 16, height = 16)
opar <- par(mar = c(0,0,0,0), bg = "#15191E")
plot(
  c(-100, 100), c(-100, 100), type = "n", xaxs= "i", yaxs = "i",
  xlab = NA, ylab = NA, axes = FALSE, asp = 1
)
rasterImage(image, -100, -100, 100, 100)
par(opar)
dev.off()
rsvg::rsvg_png("x.svg", "BesselJ-nu3.png", width = 512, height = 512)
