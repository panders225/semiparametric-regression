###########################################################
# This is an example of using Cox regression, using a 
# data set that exists within the survival package 
###########################################################
###########################################################
# Clear the workspace. I do this every time
###########################################################
#rm(list = ls())
###########################################################
# Set the seed. I also do this every time
###########################################################
#set.seed(4428967)
###########################################################
# Set the working directory. 
###########################################################
#setwd("C:\\Users\\Carroll\\Documents\\My_Documents\\2018_SemiPar\\Data_Sets\\Cox_Regression")
###########################################################
# Load the libraries
###########################################################
# install.packages("survival")
library(survival)
library(mgcv)
###########################################################
# Get the data. status1 is the indicator of an event: 
# = 1 for an event, = 0 for censored
###########################################################
data(pbc)

names(pbc)
pbc$status1 = as.numeric(pbc$status==2)
cat('The sample size = ',length(pbc$age),"\n")
cat('The number of uncensored subjects = ',sum(pbc$status1),"\n")
head(pbc)
###########################################################
# Run a simple Cox model on all the variables
###########################################################
pbc$stage = factor(pbc$stage)
b0 = gam(time ~ trt+sex+stage+s(sqrt(protime))+s(platelet)+
            s(age)+s(bili)+s(albumin)+s(sqrt(ast))+s(alk.phos),
          weights=status1,family=cox.ph,data=pbc)
summary(b0)
###########################################################
# After studying the summary, Simon Wood removes a
# number of nonsignificant variables. and refits. Here
# is the Cox regression on the seleted variables
###########################################################
b = gam(time ~ trt + sex + s(sqrt(protime)) + s(platelet) + s(age) +
  s(bili) + s(albumin),weights=status1,family=cox.ph,data=pbc)
summary(b)
###########################################################
# This program is concerned with Section 7.8 of
# Wood's second edition. It provide the 6 plots in 
# Figure 7.33 of that book, 5 smooths and a residual
# plot. That Figure is a 3x2 graph, all but the
# residual plots being centered
###########################################################
#pdf("Cox_Model_Cirrhosis.pdf")
par(mfrow=c(3,2))
plot(b,ylab="Smooth Fit")
plot(b$linear.predictors,residuals(b),xlab="Linear predictors",
     ylab = "Cox Residuals")
#dev.off()

###########################################################
# Using the Cox fit, you can create a data from for
# any set of the predictors. Since there are 418 subjects,
# you cannot plot the fitted survival curse as a function
# of time for all the subjects, but you can do so for
# any one subject
###########################################################
## create prediction data frame...
cat('Number of subjects = ',length(pbc$sex),"\n")
# What subject are you interested in 
# Set subject number
#pdf("Survival_Probabilities_Subject_66.pdf")
subjectnum = 66
par(mfrow=c(1,1))
# Number of time point in the grid
np = 300
# Get the data frame for your chosen subject
newd = data.frame(matrix(0,np,0))
for (n in names(pbc)) 
  newd[[n]] = rep(pbc[[n]][subjectnum],np)
newd$time = seq(0,4500,length=np)
## predict and plot the survival function...
fv = predict(b,newdata=newd,type="response",se=TRUE)
plot(newd$time,fv$fit,type="l",ylim=c(0.,1),xlab="time",
     ylab="survival",lwd=2,col="blue",
     main = substitute(paste("Subject = ",m),list(m = subjectnum)))
## Add intervals based on cumulative hazard s.e...
# This appears to be correct, but involves heavy theory
se = fv$se.fit/fv$fit
lines(newd$time,exp(log(fv$fit)+2*se),col="red4",lwd=2)
lines(newd$time,exp(log(fv$fit)-2*se),col="red4",lwd=2)
#dev.off()


#bselect = gam(time ~ trt+sex+stage+s(sqrt(protime))+s(platelet)+
#           s(age)+s(bili)+s(albumin)+s(sqrt(ast))+s(alk.phos),
#         weights=status1,family=cox.ph,data=pbc,select=TRUE)
# summary(bselect)


