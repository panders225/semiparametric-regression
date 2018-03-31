###########################################################
# This is the spinal bone mineral density data with
# the analysis having a random intercept
###########################################################
###########################################################
# Clear the workspace. I do this every time
###########################################################
rm(list = ls())
###########################################################
# Set the seed. I also do this every time
###########################################################
set.seed(4428967)
###########################################################
# Set the working directory. 
###########################################################
# setwd("C:\\Users\\Carroll\\Documents\\My_Documents\\2018_SemiPar\\Data_Sets\\Splnal_Bone_Mineral_Density")
###########################################################
# Load the libraries
###########################################################
library(HRW)
library(mgcv)
###########################################################
# Load the data. 
###########################################################
data(femSBMD)
help(femSBMD)
###########################################################
# Names of the variables
###########################################################
names(femSBMD)
###########################################################
# Number of unique people
###########################################################
cat('The number of subjects = ',length(unique(femSBMD$idnum)),"\n")
print(length(unique(femSBMD$idnum)))
str(femSBMD)

###########################################################
# Run the simple random intercept code
###########################################################
fit <- gamm(spnbmd ~ s(age) + black + hispanic + white
            , random = list(idnum = ~1)
            ,data = femSBMD
            )

###########################################################
# Run a Summary
###########################################################
summary(fit$gam)
plot(fit)
###########################################################
# Run the simple random intercept code but with a cubic
# spline and k=15
###########################################################
fitcr15 <- gamm(spnbmd ~ s(age,bs="cr",k=15) + black + hispanic + white,
            random = list(idnum = ~1),data = femSBMD)
###########################################################
# Run a Summary
###########################################################
summary(fitcr15$gam)
###########################################################
# Run the simple random intercept code but with a cubic
# spline and k=15, also using REML
###########################################################
fitcr15REML <- gamm(spnbmd ~ s(age,bs="cr",k=15) + black + hispanic + white,
                random = list(idnum = ~1),data = femSBMD,method="REML")
###########################################################
# Run a Summary
###########################################################
summary(fitcr15REML$gam)
###########################################################
# Vizualize the fit to age with the default code
# The function you see is centered to have sample mean = 0
# This is a default thing for when you have more than 
# one variable fitted by a spline, because it ensures
# that the model is identified
###########################################################
#pdf("Spinal_Data_Age_Fit.pdf")
par(mai=c(1.02,0.9,0.5,0.3))
par(mfrow=c(1,1))
plot(fit$gam,cex.lab = 2,cex.axis = 1.5,shade = TRUE,
shade.col = "palegreen",ylab="fitted splines",main='SpinalBone Mineral Density Data')
#dev.off()

###########################################################
# Now get some confidence intervals
# The first 4 intervals are self-explanatory
# The 5th I do not know what that means
# I also do not known what sd(Xr - 1) means
# sd(Intercept) is \sigma_U
# Within group standard error is \sigma_{\epsilon}
###########################################################
intervals(fit$lme)
###########################################################
# Now test whether ethnicity has any effect
###########################################################
# Fit the reduced model
fitReduced = gamm(spnbmd ~ s(age),random = list(idnum = ~1),
                   data = femSBMD)
# Then construct the likelihood ratio test statistic
logLRstat = 2*(fit$lme$logLik - fitReduced$lme$logLik)
# Get the p-value
pValue = 1 - pchisq(logLRstat,df = 3)
# Print the p-value
pValue
cat('The p-value for no effect of ethnicity = ',pValue,"\n")
