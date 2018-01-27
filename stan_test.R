

system.file("demo", "WarsawAptsBayes.R", package="HRW")

##################
# import third-party libraries
##################
library("HRW")

##################
# Bring in the data
##################

xOrig <- WarsawApts$construction.date
yOrig <- WarsawApts$price.m2
mean.x <- mean(xOrig)
sd.x <- sd(xOrig)
mean.y <- mean(yOrig)
sd.y <- sd(yOrig)
x <- (xOrig - mean.x)/sd.x
y <- (yOrig - mean.y)/sd.y



