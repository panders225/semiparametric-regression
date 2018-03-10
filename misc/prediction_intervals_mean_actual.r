###########################################################
# Salinity Data
#
# salinity is the water salinity in the estuaries of 
# the Pamlico Sound
#
# lagged.salinity is the same thing 1 month earlier
#
# trend is the indicator of the month
#
# discharge is the river discharge at the Neuse River
# and the Tar River: it is a composite measure
###########################################################
# Clear the workspace. I do this every time
###########################################################
rm(list = ls())
###########################################################
# Set the seed. I also do this every time
###########################################################
set.seed(4428967)
###########################################################
# Set the working directory. I probably do not need to 
# do this (it will not work in Linux), but I do to keep
# my blood pressure down
###########################################################
setwd("C:\\Users\\Carroll\\Documents\\My_Documents\\2018_SemiPar\\Data_Sets\\Salinity")
###########################################################
# Get the modified data and plot it
# Plot the salinity versus the discharge. The idea is that
# as more water flows into the estuary, the salinity
# should decrease
###########################################################
thedata   = read.csv(file='Salinity_Data_Modified_Leverage_Points.csv')
salinity  = thedata$salinity
lagged    = thedata$lagged.salinity
trend     = thedata$trend
discharge = thedata$discharge
salinity  = as.vector(salinity)
discharge = as.vector(discharge)
n         = length(salinity)
ord       = order(discharge)
x         = discharge[ord]
y         = salinity[ord]
discharge = x
salinity  = y
###########################################################
# Get the 95% pointwise confidence interval for the 
# mean of Y give X, and then the 95% confidence interval 
# for the actual values of Y given X
###########################################################
t = qt(0.975,df=n-2)
###########################################################
# Get the 95% pointwise confidence interval for the 
# actual value of Y give X
###########################################################
# Fit the gam
##  Fit gam
library(mgcv)
fitgam = gam(salinity ~ s(discharge,k=20))
##  Compute predictions and their standard errors
predgam = predict(fitgam,se.fit=TRUE)
upperCI = predgam$fit+t*predgam$se.fit
lowerCI = predgam$fit-t*predgam$se.fit
##  sigmahat2 is the estmate of residual variance
sigmahat2 = fitgam$sig2
##  Compute estimates of standard deviations 
## of predictions of actual y's
se.pred = sqrt(predgam$se.fit^2 + fitgam$sig2)
##  Get t-value for confidence intervals 
## for the actual value of Y given X
##  Confidence interval lower limits
lower_actual_CI = predgam$fit - (t * se.pred)
##  Upper limits
upper_actual_CI = predgam$fit + (t * se.pred)
# Do the same thing for the mean of Y
upperCI = predgam$fit+t*predgam$se.fit
lowerCI = predgam$fit-t*predgam$se.fit
#Now plot all that stuff
par(mfrow=c(1,1))
#pdf("Salinity_Mean_Actual_CI.pdf")
plot(discharge,fitgam$fit,type="l",lty=1,col="black",lwd=3,
     xlab="Discharge",ylab="Salinity",main="Pamlico Sound, Two CI",
     ylim=c(min(lower_actual_CI),max(upper_actual_CI)))
lines(discharge,upperCI,type="l",lty=2,col="blue",lwd=3)
lines(discharge,lowerCI,type="l",lty=2,col="blue",lwd=3)
lines(discharge,lower_actual_CI,type="l",lty=2,col="red",lwd=3)
lines(discharge,upper_actual_CI,type="l",lty=2,col="red",lwd=3)
legend(25,18,legend = c("Mean","Actual"),
       lty = rep(1,2),lwd = rep(2,2),col = c("blue","red"),cex = 1)
#dev.off()
