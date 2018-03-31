###########################################################
# This is the spinal bone mineral density data with
# the analysis having a random intercept
#
# In this example, the out outcome is whether the 
# spine mineral density is 
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
#setwd("C:\\Users\\Carroll\\Documents\\My_Documents\\2018_SemiPar\\Data_Sets\\Splnal_Bone_Mineral_Density")
###########################################################
# Load the libraries
###########################################################
library(HRW)
library(mgcv)
###########################################################
# Load the data. 
###########################################################
data(femSBMD)
###########################################################
# Create a variable with low sbmd
###########################################################
femSBMD$spnbmdlow = (femSBMD$spnbmd < 0.75)
aa = sum(femSBMD$low)
cat('The number of observations with low sbmd = ',aa,"\n")
###########################################################
# Names of the variables
###########################################################
names(femSBMD)
###########################################################
# Number of unique people
###########################################################
cat('The number of subjects = ',length(unique(femSBMD$idnum)),"\n")
print(length(unique(femSBMD$idnum)))
###########################################################
# Run the simple random intercept code
###########################################################
fit <- gamm(spnbmdlow ~ s(age) + black + hispanic + white,
            random = list(idnum = ~1),
            data = femSBMD,family=binomial(link="logit"))
###########################################################
# Run a Summary
###########################################################
summary(fit$gam)
###########################################################
# Vizualize the fit to age with the default code
# The function you see if centered to have sample mean = 0
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
# 
# The which="fixed" term seems to make the program run
###########################################################
intervals(fit$lme,which="fixed")
###########################################################
# Now test whether ethnicity has any effect
# This is a disaster
###########################################################
# Fit the reduced model
fitReduced = gamm(spnbmdlow ~ s(age),random = list(idnum = ~1),
                   data = femSBMD,family=binomial(link=logit))
# Then construct the likelihood ratio test statistic
logLRstat = 2*(fit$lme$logLik - fitReduced$lme$logLik)
cat('The loglikelihood ratio statistics = ',logLRstat,"\n")
pValue = 1 - pchisq(logLRstat,df = 3)
# Print the p-value
pValue
cat('The p-value for no effect of ethnicity = ',pValue,"\n")
###########################################################
# Now test whether ethnicity has any effect
# Here you have to use the Hotelling Tsquared test, which 
# is asymptotically a chisquared with 3 degrees of freedom
###########################################################
betahat = fit$lme$coefficients$fixed[2:4]
betahat = as.vector(betahat)
covmat  = fit$lme$varFix[2:4,2:4]
covmat
icovmat = solve(covmat)
Hotelling = t(betahat) %*% icovmat %*% betahat
pValue = 1 - pchisq(Hotelling,df = 3)
cat('Hotelling T2 p-value for no effect of ethnicity = ',pValue,"\n")
