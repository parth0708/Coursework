plotplane <- function(z){
  x <- z[1]
  y <- z[2]
  c1 <- z[3]
  m <- -x/y
  vhmod <- 1/sqrt(x^2 + y^2)
  c <- -sign(y)*c1*vhmod*sqrt(1 + m^2)
  abline(a = c, b = m)
}