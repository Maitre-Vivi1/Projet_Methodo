rm(list=ls())


lass <- function(x) {
  y = 0
  if(x <= -1){
    y = x + 1
  }
  if(x>= 1){
    y = x - 1
  }
  return(y)
}


abc <- seq(-10,10,0.001)

abc <- lapply(abc, lass)
abc
plot(seq(-10,10,0.001), abc, "l")



data("CO2")
data("diamonds")
