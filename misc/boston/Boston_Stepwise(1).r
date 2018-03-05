################################################################################
# This is the Boston Housing Data, and an example of stepwise regression
#
# It is probably not going to be very informative, because all the variables
# are statistically significant
################################################################################

################################################################################
# Clear the workspace
################################################################################
rm(list = ls())
################################################################################
# Set the seed
################################################################################
set.seed(4428967)
################################################################################
# Set the working directory. Not needed here since the Boston
# data is in the HRW pacvkage
################################################################################
# Mini Laptop
#setwd("C:\\Users\\Raymond\\Documents\\SemiPar_2018\\Data_Sets\\Boston_Housing")
# Big Laptop
#setwd("C:\\Users\\Carroll\\Documents\\My_Documents\\2018_SemiPar\\Data_Sets\\Boston_Housing")
################################################################################
# Load HRW and gam packages. DO NOT LOAD mgcv YET
################################################################################
library(HRW)
library(gam)
#library(mgcv)
###########################################################
# If you have played with mgcv as part of a session,
# you need to detach it
###########################################################
detach("package:mgcv", unload=TRUE)
################################################################################
# Load BostonMortgages data set from the HRW package
################################################################################
data(BostonMortgages)
################################################################################
# Get rid of the family with a debt to income ratio > 1.5
################################################################################
BostonMortgages = BostonMortgages[BostonMortgages$dir < 1.5,]
################################################################################
# Fit using all the variables as linear terms. This serves as a baseline
################################################################################
fitInit = gam(deny ~ black  + dir + lvr
              + pbcr + self + single +  as.factor(ccs),
              family = binomial,data = BostonMortgages)
################################################################################
# Now code and run the stepwise regression
# The statement s(x,5) means the effective degrees of freedom = 5
################################################################################
stepFit = step.gam(fitInit,scope =
                      list("black" = ~1 + black,
                           "pbcr" = ~1 + pbcr,
                           "self"  = ~1 + self,
                           "single" = ~1 + single,
                           "ccs"  = ~1 + as.factor(ccs),
                           "dir"  =  ~1 +  dir + s(dir,5),
                           "lvr"  = ~1 + lvr + s(lvr,5)),
                   family = binomial,data = BostonMortgages)
################################################################################
# What is the final model?
################################################################################
print(names(stepFit$"model")[-1])
################################################################################
# If this were a real example, HRW recommend that you go to mgcv and
# run the final model. Notice the mgcv:::gam statement
################################################################################
library(mgcv)
fitFinal = mgcv:::gam(deny ~ black + s(dir,k=20) + s(lvr,k=20)
                      + pbcr + self + single + as.factor(ccs),
                      family = binomial,data = BostonMortgages)
summary(fitFinal)
