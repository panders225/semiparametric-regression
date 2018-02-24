################################################################################
# This is the Boston Housing Data, with a simple ANOVA without an interaction
# There are many possible confounders, but in this one  I simply use Z = credit score
# Plot curves for black and non-black borrowers, with CIs
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
# Set the working directory
################################################################################
setwd("C:\\Users\\Raymond\\Documents\\SemiPar_2018\\Data_Sets\\Boston_Housing")
################################################################################
# Load HRW and mgcv packages
################################################################################
library(HRW)
library(mgcv)


################################################################################
# Load BostonMortgages data set from the HRW package
################################################################################
data(BostonMortgages)
################################################################################
# Get rid of the family with a debt to income ratio > 1.5
################################################################################
NewBostonMortgages = BostonMortgages[BostonMortgages$dir < 1.5,]
dir = NewBostonMortgages$dir
################################################################################
# Fit using all the variables
################################################################################
fit1GAMBostMort = gam(deny ~ black + s(dir) + s(lvr)
                      + pbcr + self + single + as.factor(ccs),
                      family = binomial(link="logit"),    data = BostonMortgages)
################################################################################
# Get a summary to see what is statistically significant and to
# interpret the signs of the dummy variables
################################################################################
summary(fit1GAMBostMort)

