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
library(mgcv)
###########################################################
# Get the data 
###########################################################
thedata   = read.csv(file='lidar.csv')
range     = thedata$range
logratio  = thedata$logratio
n         = length(range)
###########################################################
# Scatterplot of the data
###########################################################
#pdf("lidar_scatterplot.pdf")
plot(range,logratio,type="p",pch='*',main="LIDAR Data",
     xlab="range",ylab="logratio",cex=2)
#dev.off()
###########################################################
# Linear Regression, its summary, and the variable names
# The summary gives the p-values and the Rsquare
###########################################################
lidar_mod = lm(logratio~range)
summary(lidar_mod)
names(lidar_mod)
###########################################################
# Add the fit to the scatterplot
###########################################################
#pdf("lidar_scatterplot_withfit.pdf")
plot(range,logratio,type="p",pch='*',main="LIDAR Data",
     xlab="range",ylab="logratio",cex=2)
lines(range,fitted(lidar_mod),lwd=3,col="blue")
#dev.off()
###########################################################
# Ordinary residual plot
###########################################################
#pdf("lidar_residual_plot.pdf")
plot(fitted(lidar_mod),residuals(lidar_mod),xlab="fitted values",
     ylab="residuals",pch='*',cex=2,main="LIDAR Data")
#dev.off()


###########################################################
############ Generalized Additive Models ##################
###########################################################

###########################################################
# Default values for GCV with a spline smooth on range.
# "s(range)" defaults to thin-plate splines.
# Default k appears to be 10 for thin-plate and cubic splines.
# s(range,bs="cr) gives the cubic spline as covered in 
# class
###########################################################
gam_default = gam(logratio~s(range,bs="cr"), data=thedata)
summary(gam_default)
###########################################################
# Add the fit to the scatterplot
###########################################################
#pdf("lidar_default_gam_fit.pdf")
plot(range, logratio, type="p", pch='*', 
     main="LIDAR Data, default GAM fit",
     xlab="range", ylab="logratio", cex=2)
lines(range, fitted(gam_default), lwd=3, col="blue")
#dev.off()
###########################################################
# Absolute residual plot with a gam line fit to it
# to check for heteroscedasticity
###########################################################
#pdf("lidar_default_gam_absoluteresiduals_fit.pdf")
plot(fitted(gam_default), abs(gam_default$residuals),
     ylab="Absolute Residuals", 
     main="Absolute residuals: default GAM fit of LIDAR data",
     xlab="Predicted Values")
x    = fitted(gam_default)
y    = abs(gam_default$residuals)
gam_default_abs = gam(y~s(x,bs="cr"), data=thedata)
lines(x,fitted(gam_default_abs),lwd=3,col="blue")
#dev.off()
###########################################################
# The gam.check() function describes the model fit and 
# produces diagnostic plots.  
# For each smooth we see the nominal number of basis 
# functions (k'), and the effective degrees of freedom (edf).
###########################################################
gam.check(gam_default)
###########################################################
# Smoothing parameter chosen by GCV
###########################################################
gam_default$sp
###########################################################
# Repeat GAM fit with k=5: 4 basis functions and one df 
# for identifiability
###########################################################
pdf("lidar_k5_gam_fit.pdf")
gam_k5 = gam(logratio~s(range, k=5,bs="cr"), data=thedata)
plot(range, logratio, type="p", pch='*', 
     main="LIDAR Data, k=5 GAM fit",
     xlab="range", ylab="logratio", cex=2)
lines(range, fitted(gam_k5), lwd=3, col="blue")
dev.off()
# Smoothing parameter
gam_k5$sp
# Check effective degrees of freedom
gam.check(gam_k5)


###########################################################
# Absolute Residual Plot
###########################################################
pdf("lidar_k5_gam_absoluteresiduals_fit.pdf")
plot(fitted(gam_k5), abs(gam_k5$residuals),
     ylab="Absolute Residuals", 
     main="Absolute residuals: k=5 GAM fit of LIDAR data",
     xlab="Predicted Values")
x    = fitted(gam_k5)
y    = abs(gam_k5$residuals)
gam_k5_abs = gam(y~s(x,bs="cr"), data=thedata)
lines(x,fitted(gam_k5_abs),lwd=3,col="blue")
dev.off()

###########################################################
# Repeat GAM fit with k=20.
# Thin-plate splines have good properties, but are slower
# to fit than cubic splines.  With a big gata set, try
# s(range, k=20, bs="cr") to fit cubic splines with k=20.
###########################################################
pdf("lidar_k20_gam_fit.pdf")
gam_k20 = gam(logratio~s(range, k=20,bs="cr"), data=thedata)
plot(range, logratio, type="p", pch='*', 
     main="LIDAR Data, k=20 GAM fit",
     xlab="range", ylab="logratio", cex=2)
lines(range, fitted(gam_k20), lwd=3, col="blue")
dev.off()
# Smoothing parameter
gam_k20$sp
# Check effective degrees of freedom
gam.check(gam_k20)

# Absolute Residual Plot
pdf("lidar_k20_gam_absoluteresiduals_fit.pdf")
plot(fitted(gam_k20), abs(gam_k20$residuals),
     ylab="Absolute Residuals", 
     main="Absolute residuals: k=20 GAM fit of LIDAR data",
xlab="Predicted Values")
x    = fitted(gam_k20)
y    = abs(gam_k20$residuals)
gam_k20_abs = gam(y~s(x,bs="cr"), data=thedata)
lines(x,fitted(gam_k20_abs),lwd=3,col="blue")
dev.off()

###########################################################
# Repeat GAM fit without penalization (option fx=TRUE).
###########################################################
pdf("lidar_nopenalty_gam_fit.pdf")
gam_fixed = gam(logratio~s(range, k=20, fx=TRUE,bs="cr"), data=thedata)
gam.check(gam_fixed)
plot(range, logratio, type="p", pch='*', 
     main="LIDAR Data, unpenalized GAM fit",
     xlab="range", ylab="logratio", cex=2)
lines(range, fitted(gam_fixed), lwd=3, col="blue")
dev.off()
###########################################################
# Smoothing parameter not used
###########################################################
gam_fixed$sp
gam.check(gam_fixed)
plot(fitted(gam_fixed), abs(gam_fixed$residuals),
     ylab="abs(residuals)", 
     main="Absolute residuals: unpenalized GAM fit of LIDAR data")





var_ratio_default = (max(abs(fitted(gam_default_abs))) / min(abs(fitted(gam_default_abs))))^2
var_ratio_k5 = (max(abs(fitted(gam_k5_abs))) / min(abs(fitted(gam_k5_abs))))^2
var_ratio_k20 = (max(abs(fitted(gam_k20_abs))) / min(abs(fitted(gam_k20_abs))))^2
cat('Default fit, maximum to minimum variance ratio = ',var_ratio_default,"\n")
cat('k = 5   fit, maximum to minimum variance ratio = ',var_ratio_k5,"\n")
cat('k = 20  fit, maximum to minimum variance ratio = ',var_ratio_k20,"\n")
