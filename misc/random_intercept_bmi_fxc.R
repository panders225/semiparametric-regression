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
setwd("C:\\Users\\Carroll\\Documents\\My_Documents\\2018_SemiPar\\Data_Sets\\Pepe_BMI_Data")
###########################################################
# Load the libraries
###########################################################
library(HRW)
library(mgcv)
###########################################################
# Load the data. 
###########################################################
bmidata = read.csv(file="pepe_wangcube_data_Filtered.csv")
###########################################################
# Names of the variables
###########################################################
names(bmidata)
#id = bmidata$id
#bmi=bmidata$bmi
#age=bmidata$age
#gender = bmidata$gender
#obes_adu = bmidata$obesadu
###########################################################
# Number of unique people
###########################################################
cat('The number of subjects = ',length(unique(bmidata$id)),"\n")
print(length(unique(bmidata$id)))
###########################################################
# Run the simple random intercept code
###########################################################
fit <- gamm(bmi ~ s(age) + gender + obes_adu,
            random = list(id = ~1),data = bmidata)
###########################################################
# Run a Summary
###########################################################
summary(fit$gam)
# Vizualize the fit to age with the default code
# The function you see is centered to have sample mean = 0
# This is a default thing for when you have more than 
# one variable fitted by a spline, because it ensures
# that the model is identified
###########################################################
#pdf("BMI_Data_Age_Fit.pdf")
par(mai=c(1.02,0.9,0.5,0.3))
par(mfrow=c(1,1))
plot(fit$gam,cex.lab = 2,cex.axis = 1.5,shade = TRUE,
shade.col = "palegreen",ylab="fitted splines",main="BMI Data")
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
# Now test whether either gender or being obese in adulthood
###########################################################
# Fit the reduced model
fitReduced = gamm(bmi ~ s(age),random = list(id = ~1),
                   data = bmidata)
# Then construct the likelihood ratio test statistic
logLRstat = 2*(fit$lme$logLik - fitReduced$lme$logLik)
# Get the p-value
pValue = 1 - pchisq(logLRstat,df = 3)
# Print the p-value
pValue
cat('The p-value for no effect of ethnicity = ',pValue,"\n")
###########################################################
# Now see if we need different fit for different
# adult obestity status
###########################################################
fitcomplex <- gamm(bmi ~ s(age,by=as.factor(obes_adu)) + gender + obes_adu,
            random = list(id = ~1),data = bmidata)
summary(fitcomplex$lme)
summary(fitcomplex$gam)
qq = anova(fitReduced$lme,fitcomplex$lme)
qq
names(qq)
cat('The p-value for no effect of different smooths = ',qq$`p-value`[2],"\n")

#logLRstatcomplex = 2*(fitcomplex$lme$logLik - fit$lme$logLik)
## Get the p-value
#pValue = 1 - pchisq(logLRstatcomplex,df = 3)
## Print the p-value
#pValue
#cat('The p-value for no effect of different smooths = ',pValue,"\n")
