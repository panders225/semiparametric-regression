###########################################################
# This is the Warsaw Apartment Data in a semiparametric
# ANCOVA without any Interactions
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
setwd("C:\\Users\\Raymond\\Documents\\SemiPar_2018\\Data_Sets\\Warsaw.Data")
###########################################################
# Get the mgcv package
###########################################################
library(HRW)
library(mgcv)
###########################################################
# Get the Warsaw Data and name the crucial variables
###########################################################
data(WarsawApts)
thedate = WarsawApts$construction.date
areaPerMzloty = WarsawApts$areaPerMzloty
###########################################################
# Check out the various districts
###########################################################
district=WarsawApts$district
###########################################################
# The first district named is the default
# the other are compared to that district. 
# In this example, the reference district is Mokotow
###########################################################
cat('Here are the different districts',"\n")
cat('The first one named is the reference district',"\n")
unique(district)
###########################################################
# Run the model without an interaction
###########################################################
Wancova = gam(areaPerMzloty~factor(district)
              +s(thedate,bs="cr",k=20),method="REML")
###########################################################
# Summarize the model
###########################################################
summary(Wancova)
###########################################################
# Run the model with an interaction. Please note that
# the factor(district)*thedate code includes all the
# non-spline terms in the model
###########################################################
fitSimpSemiInt = 
  gam(areaPerMzloty ~
      factor(district)*thedate 
       + s(thedate,bs = "cr",k = 23),
        data = WarsawApts)
###########################################################
# Summarize the interaction model. You will see
# that the only thing that seems to be statistically 
# significant is the construction data: Srodmiescie
# seems to have disappeared
###########################################################
summary(fitSimpSemiInt)
###########################################################
# Given the puzzling results, it seems like a good idea
# to compare the interaction and non-interaction
# models!
###########################################################
anova(Wancova,fitSimpSemiInt,test="F")
###########################################################
# Next we test whether the functions for Srodmiescie
# and the other districts are different
#
# This is a test of any difference. A constant difference
# is one of the possibilities
###########################################################
# First fit the same function to all 4 districts
# This is the null model
fit1 = gam(areaPerMzloty ~ s(thedate))
# Define the district Srod
IndSod = as.numeric(district == "Srodmiescie")
# Next fit a separate function to district Srod
# and a common model to the other 3 districts
# This is the alternatie model
fit2 = gam(areaPerMzloty ~  s(thedate,IndSod))
# Get the summary
summary(fit2)
# Do the test
anova(fit1,fit2,test="F")
