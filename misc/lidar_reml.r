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
# In this analysis, we did two fits
#
# We then fit a cubic basis function spline for k=20
# using GCV and the same thing using REML
#
# The intent of this exercise is to show that 
# REML and GCV are different, but not much
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
library(mgcv)
###########################################################
# Get the data 
###########################################################
thedata   = read.csv(file='lidar.csv')
range     = thedata$range
logratio  = thedata$logratio
n         = length(range)
###########################################################
# Fit the model with the default, gcv
###########################################################
gamk20GCV = gam(logratio~s(range, k=20,bs="cr"),
              data=thedata)
###########################################################
# Give a summary
###########################################################
summary(gamk20GCV)
###########################################################
# Get lambda
###########################################################
gamk20GCV$sp
###########################################################
# Fit the model with REML
###########################################################
gamk20REML = gam(logratio~s(range, k=20,bs="cr"),
                method="REML", data=thedata)
###########################################################
# Give a summary
###########################################################
summary(gamk20REML)
###########################################################
# Get lambda
###########################################################
gamk20REML$sp
###########################################################
# Add the REML fit to the scatterplot
###########################################################
#pdf("lidar_gam_fit_GCV_REML.pdf")
plot(range, logratio, type="p", pch='*', 
     main="LIDAR Data",
     xlab="range", ylab="logratio", cex=2)
lines(range, fitted(gamk20GCV), lwd=3, col="blue")
lines(range, fitted(gamk20REML), lwd=3, col="red")
legend(400,-0.6,legend=c("GCV","REML"),lty=1,
           col=c(4,2))
#dev.off()
###########################################################
# Shift the REML fit so you can see how close they are
###########################################################
#pdf("lidar_gam_fit_GCV_REML_Shift.pdf")
plot(range, logratio, type="p", pch='*', 
     main="LIDAR Data, Shift REML by 0.05",
     xlab="range", ylab="logratio", cex=2)
lines(range, fitted(gamk20GCV), lwd=3, col="blue")
lines(range, fitted(gamk20REML)+0.05, lwd=3, col="red")
legend(400,-0.6,legend=c("GCV","REML"),lty=1,
       col=c(4,2))
#dev.off()

###########################################################
# Show how to predict the mean at points not in the
# actual data, in this case at 450 and 650
###########################################################
lidarNEW = data.frame(range = c(450,650))
qq = predict(gamk20REML,as.data.frame(lidarNEW),se.fit = TRUE)
cat('Fit at X=450 = ',qq$fit[1],"\n")
cat(' SE at X=450 = ',qq$se.fit[1],"\n")
cat('Fit at X=650 = ',qq$fit[2],"\n")
cat(' SE at X=650 = ',qq$se.fit[2],"\n")
###########################################################
# Now get the fitted lines with variability bounds, 
# and plot. These are wrong of course because
# of the heterosce3dasticity in the LIDAR data
###########################################################
pdf("LIDAR_Fit_Bounds.pdf")
qq = predict(gamk20REML,se.fit = TRUE)
upper = qq$fit + (1.96 * qq$se.fit)
lower = qq$fit - (1.96 * qq$se.fit)
myfit = qq$fit
plot(range,myfit,type="l",col="blue",lwd=3,xlab="Range",
     ylab="Logratio",
     main="LIDAR")
polygon(x=c(range, rev(range)), y=c(upper, rev(lower)), 
        col="gray", border=NA)
lines(range,myfit,col="blue",lwd=3)
dev.off()
