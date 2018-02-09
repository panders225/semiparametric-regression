###########################################################
# This is the lidar data set. 
#
# The technique known as LIDAR (light detection 
# and ranging) uses the reflection of laser-emitted 
# light to detect chemical compounds in the atmosphere.
# The LIDAR technique has proven to be an efficient tool 
# for monitoring the distribution of several atmospheric 
# pollutants of importance
# 
# The X variable is range: the distance traveled before 
# light is reflected back to its source
#
# The Y variable is logratio: the logarithm of the ratio 
# of received light from two laser sources 
#
# In this analysis, we firt fit a simple linear model,
# which we show to be ridiculously inadequate
#
# We then fit a cubic basis function spline for k=5, k=20
# and k = 10 (the default). There is also a function
# that checks whether k is large enough. In this particular
# data set, setting k = 4 gives a p-value + 0, while 
# setting k = 5 gives a p-value of 0.54.
# 

###########################################################
# Clear the workspace
rm(list = ls())
###########################################################
# Set the seed
###########################################################
set.seed(4428967)
###########################################################
# Set the working directory
###########################################################
#setwd("C:\\Users\\Carroll\\Documents\\My_Documents\\2018_Semi_in_R\\Data_Sets\\LIDAR")
setwd("C:\\Users\\Raymond\\Documents\\SemiPar_2018\\Data_Sets\\LIDAR")
###########################################################
# Get the mgcv package
###########################################################
library(HRW)
###########################################################
# Get the data 
###########################################################
thedata   = read.csv(file='lidar.csv')
range     = thedata$range
logratio  = thedata$logratio
n         = length(range)
###########################################################
# First Code Snippet
###########################################################
x <- range
y <- logratio
# Set the number of Interior Knots
numIntKnots <- 20
# Get the interior knots at the quantiles
intKnots <- quantile(unique(x),seq(0,1,length=
                                     (numIntKnots+2))[-c(1,(numIntKnots+2))])
# Get some values just < min(x) and just > max(x)
a <- 1.01*min(x) - 0.01*max(x) 
b <- 1.01*max(x) - 0.01*min(x)
# Create the O'Sullivan Basis Functions
Z <- ZOSull(x,range.x = c(a,b),intKnots = intKnots)
# Get ready to use the nonlinear mixed effects library
library(nlme)
cat('The number of basis functions = ',dim(Z)[2],"\n")
# Create a vector of ones for the intercept
dummyID <- factor(rep(1,length(x)))
# Now fit a linear mixed effects model but do not have a 
# random intercept since there is only 1 group
# The Pdident says that the random effects are all 
# independent with the same variance. The ~-1
# but without a column of ones
fit <- lme(y ~ x,random = list(dummyID = pdIdent(~-1+Z)))
# Get the fixed effects. 
betaHat <- fit$coef$fixed ; 
# Get the estimated random effects. HRW says that the
# nomenclature converts the object fit$coef$random from an R
# list to an R array
uHat <- unlist(fit$coef$random)

###########################################################
# Now get an print sigma^2_e, sigma^2_u, and the
# ratio of the former to the latter, which is lambda
###########################################################
sigsqepsHat <- fit$sigma^2
sigsquHat <- as.numeric(VarCorr(fit)[1,1])
outVec <- c(sigsqepsHat,sigsquHat,sigsqepsHat/sigsquHat)
print(round(as.numeric(outVec),2))
###########################################################
# Now let us plot the fit on a grid
###########################################################
# Set the length of the grid
ng <- 1001 ; 
# create the grid where you want to plot the function
xg <- seq(a,b,length=ng)
# Define the nx2 matrix with first column = 1 and 
# second column the x-values on the grid
Xg <- cbind(rep(1,ng),xg) 
# Now define the O'Sullivan basis functions for the
# grid
Zg <- ZOSull(xg,range.x = c(a,b),intKnots = intKnots)
# Get the fiten function on the grid
fHatg <- as.vector(Xg%*%betaHat + Zg%*%uHat)
# Plot the data points
plot(x,y,xlab = "Range",
     ylab = "Log Ratio",
     main = "LIDAR Data, brute force Spline",
     col = "blue",cex.lab = 1.5,cex.axis = 1.5)
lines(xg,fHatg,col = "red",lwd = 3)


