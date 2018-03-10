###########################################################
# This is the lidar data set. 
#
#  The goal is to show how to do a simple weighted analysis
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
setwd("C:\\Users\\Carroll\\Documents\\My_Documents\\2018_SemiPar\\Data_Sets\\LIDAR")
#setwd("C:\\Users\\Raymond\\Documents\\SemiPar_2018\\Data_Sets\\LIDAR")
###########################################################
# Get the mgcv package and the gaulss package
###########################################################
library(mgcv)
###########################################################
# Get the data 
###########################################################
thedata   = read.csv(file='lidar.csv')
thedata = read.csv('/Users/panders2/Documents/schools/tamu/stat_689/homework/semiparametric-regression/misc/lidar/lidar.csv')
range     = thedata$range
logratio  = thedata$logratio
n         = length(range)
###########################################################
# Fit a default gam to the data
###########################################################
gam_default = gam(logratio~s(range,bs="cr"), data=thedata)
###########################################################
# Fit a smooth to the absolute residuals
###########################################################
x    = range
y    = abs(gam_default$residuals)
gam_default_abs = gam(y~s(x,bs="cr"), data=thedata)
plot(x,fitted(gam_default_abs),lwd=3,type='l',col='blue')
plot(gam_default)
###########################################################

# Form the weights
###########################################################
weight = 1 / (fitted(gam_default_abs) ^ 2)
###########################################################
# Rerun with a weighted Fit
###########################################################
gam_weighted = gam(logratio~s(range,bs="cr"), data=thedata,weight=weight)
###########################################################
# Plot the fit and compare to the earler unweighted fit
###########################################################
plot(x,fitted(gam_weighted),lwd=3,col="blue",type='l')
lines(x,fitted(gam_default),lwd=3,col="red",type='l')
###########################################################
# Plot the predictors against the weighted residuals
###########################################################
#pdf('LIDAR_Weighted_Residual_plot.pdf')
par(mfrow=c(1,1))
plot(x,abs(gam_weighted$residuals)/fitted(gam_default_abs),lwd=3)
#dev.off()
###########################################################
# Compare the Weighted and Unweighted Fits
###########################################################
#pdf('LIDAR_weighted_unweighted.pdf')
par(mfrow = c(1,2),cex.lab = 1.5,
    cex.main = 1.5,col.main = "navy",lwd = 2,
    mai = c(0.9,0.7,0.35,0.05))

plot(gam_weighted,shade = TRUE,shade.col = "palegreen", 
     ylab = "logratiofit", 
     trans = plogis,
     xlab = "Range",
     main = "Weighted",rug = FALSE,
     ylim=c(0.30,0.60),xlim=c(min(range),max(range)))

plot(gam_default,shade = TRUE,shade.col = "palegreen", 
     ylab = "logratio fit", 
     trans = plogis,
     xlab = "Range",
     main = "Unweighted",rug = FALSE,
     ylim=c(0.30,0.60),xlim=c(min(range),max(range)))
#dev.off()

