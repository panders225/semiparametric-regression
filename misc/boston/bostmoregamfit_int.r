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
setwd("C:\\Users\\Carroll\\Documents\\My_Documents\\2018_Semi_in_R\\Data_Sets\\Boston_Housing")
###########################################################
# Get the libraries
###########################################################
library(mgcv)  
library(HRW) 
###########################################################
# Load in data:
###########################################################
data(BostonMortgages)
###########################################################
# Get rid of the massive outlier in dir
###########################################################
dir = BostonMortgages$dir
BostonMortgages = BostonMortgages[dir<1.5,]
###########################################################
# Obtain GAM fit:
# I fit the credit score as a smooth term but with only k=4
###########################################################

fit2GAMBostMort <- gam(deny ~ black + s(dir) + s(lvr) 
                       + pbcr + self + single + s(ccs,k=4), 
                       method="REML",
                       family = binomial(link="logit"),
                       data = BostonMortgages)
###########################################################
# Now allow factor by curve interactions
###########################################################
###########################################################
# Now allow factor by curve interactions
###########################################################
fitFacByCurvBostMort <- gam(deny ~ black
                            + s(dir,by = factor(pbcr))
                            + s(lvr,by = factor(pbcr)) +
                              + pbcr + self + single + s(ccs,k = 4),
                            family = binomial,data = BostonMortgages)

summary(fitFacByCurvBostMort)

###########################################################
# Next I compare the two models. The as.factor model
# is more complex than the spline model, remember
###########################################################
anova(fit2GAMBostMort,fitFacByCurvBostMort,test="Chisq")
qq = anova(fit2GAMBostMort,fitFacByCurvBostMort,test="Chisq")
cat('Model comparison p-value = ',round(qq[2,5],2),"\n")
