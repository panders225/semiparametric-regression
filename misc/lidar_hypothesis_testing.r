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
# In this analysis, we used REML and wished to 
# test not only whether there is an effect of X,
# but also whether a linear, quadratic or cubic
# fit is sufficient
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
# Fit the model with REML
###########################################################
gamk20REML = gam(logratio~s(range, k=20,bs="cr"),
                method="REML", data=thedata)
###########################################################
# Hypothesis testing Code in HRW 
# Using mgcv as written by S. Wood
###########################################################
rangesq       = range ^ 2
rangecub      = range ^ 3
fitLinear     =  lm(logratio ~ range)
fitQuadratic  =  update(fitLinear, .~. + rangesq)
fitCubic      =  update(fitQuadratic, .~. + rangecub)
ss = anova(fitLinear,fitQuadratic,fitCubic,gamk20REML,
           test = "F")
names(ss)
ss
cat('p-value for quad   vs linear fit = ',ss$Pr[2],"\n")
cat('p-value for cubic  vs quad   fit = ',ss$Pr[3],"\n")
cat('p-value for spline vs cubic  fit = ',ss$Pr[4],"\n")
###########################################################
# Hypothesis testing Code in HRW 
# Using the Ruppert method. This is for whether
# there is no effect of range
###########################################################
library(RLRsim)
fitVIAgam = gamm(logratio~s(range,bs="cr",k=20),
                 method="REML")
print(exactRLRT(fitVIAgam$lme,nsim=50000))
