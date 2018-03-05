########## R script: Stepwise_OFPGAM.R ##########

# Stepwise fit for the Poisson generalized additive model
# fit to the physician office visits data on both the
# link scale and the response scale.
#
# I do only quasilikelihood in this analysis
# 
# Because the stepwise regression uses gam and not mgcv, you do not want 
# to get into the business of mixing them up. If you want to compare the 
# GLM fit to the mgcv GAM fit, do not do it here. This code is just
# for help with model selection.

################################################################################
# Clear the workspace
################################################################################
rm(list = ls())
################################################################################
# Set the seed
################################################################################
set.seed(4428967)
################################################################################
# Set the working directory
################################################################################
setwd("C:\\Users\\Carroll\\Documents\\My_Documents\\2018_SemiPar\\Data_Sets\\OFP_Poisson")
################################################################################
# Get the necessary files
################################################################################
library(HRW)  
library(gam)
#library(mgcv) 
library(Ecdat) 
###########################################################
# If you have played with mgcv as part of a session,
# you need to detach it
###########################################################
detach("package:mgcv", unload=TRUE)
################################################################################
# Get the Data
################################################################################
data(OFP) 
OFPforAna <- OFP
OFPforAna$age <- 10*OFPforAna$age
OFPforAna <- OFPforAna[OFPforAna$age <= 95,]
################################################################################
# Obtain an ordinary GLM fit to the variables
################################################################################
fitGLMOFP_Poisson = gam(ofp ~ age + school + adldiff +  black 
                        + sex + maried + privins + medicaid + region + hlth,
                        family = poisson, scale = -1, data = OFPforAna)
summary(fitGLMOFP_Poisson)

################################################################################
# Now try to do a stepwise GAM
################################################################################
################################################################################
# Now code and run the stepwise regression
# The statement s(x,5) means the effective degrees of freedom = 5
################################################################################
stepFit = step.gam(fitGLMOFP_Poisson, scope =
                     list("age" = ~1 + age + s(age,5),
                          "school" = ~1 + school + s(school,5),
                          "adldiff" = ~1 + adldiff,
                          "black" = ~1 + black,
                          "sex" = ~1 + sex,
                          "maried" = ~1 + maried,
                          "privins" = ~1 + privins,
                          "medicaid" = ~1 + medicaid, 
                          "region" = ~1 + region, 
                          "hlth" = ~1 + hlth))
################################################################################
# What is the final model?
################################################################################
print(names(stepFit$"model")[-1])





