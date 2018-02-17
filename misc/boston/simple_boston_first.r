###########################################################
# This is the Boston Housing Data, with a simple 
# ANOVA without an interaction
#
# There are many possible confounders, but in this one 
# I simply use Z = credit score
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
setwd("C:\\Users\\Raymond\\Documents\\SemiPar_2018\\Data_Sets\\Boston_Housing")
###########################################################
# Get the mgcv package
###########################################################
library(HRW)
library(mgcv)
###########################################################
# Get the Housing Data
###########################################################
data(BostonMortgages)
names(BostonMortgages)
###########################################################
# Get the Indicator that the subject is black
###########################################################
denyBin        = as.numeric(BostonMortgages$deny == "yes")
blackIndic     =  BostonMortgages$black == "yes"
###########################################################
# Get the odds of being denied for blacks as compared to whites
###########################################################
probabBlack    = mean(denyBin[blackIndic])
probabNotBlack = mean(denyBin[!blackIndic])
oddsBlack      =  probabBlack/(1 - probabBlack)
oddsNotBlack   =  probabNotBlack /(1 - probabNotBlack)
offa          = round(oddsBlack/oddsNotBlack,1)
cat('Odds of denial to blacks as compared to others =',
      offa,"\n")
###########################################################
# Get the ratio of debt payments to total income
###########################################################
dir = BostonMortgages$dir
###########################################################
# Get the ratio of the loan size to the assessed value of property
lvr = BostonMortgages$lvr
###########################################################
# Get the box plot of debt to income ratio
###########################################################
pdf('Boxplot_Debt_to_Income.pdf')
boxplot(dir,main='Debt to Income ratio')
dev.off()
###########################################################
# That was ugly, was it not?
###########################################################
###########################################################
# Get the box plot of loan size ratio
###########################################################
pdf('Boxplot_Loan_Size_Ratio.pdf')
boxplot(lvr,main='Loan Size to Debt ratio')
dev.off()
###########################################################
# Just keep the people with debt to income level < 1.5
###########################################################
NewBostonMortgages = BostonMortgages[BostonMortgages$dir < 1.5,] 
###########################################################
# Run the glm again from gam. This is an
# Analysis of Covariance without an interaction
###########################################################
dir = NewBostonMortgages$dir
fit2GLMBostMort <- glm(deny ~ black+dir,
                       family = binomial(link=logit),
                       data = NewBostonMortgages)
###########################################################
# Set up to plot on a grid
###########################################################
ng     = 1001
thenewDir = seq(min(dir),max(dir),length = ng)
newdataBlack = data.frame(dir=thenewDir,black="yes")
newdataNonBlack = data.frame(dir=thenewDir,black="no")
###########################################################
# Get the logits for blacks and non Blacks. The link
# statement is not needed since it is the default
###########################################################
fHatBlack = predict(fit2GLMBostMort,
                    newdata = newdataBlack,
                    type="link")
fHatNonBlack = predict(fit2GLMBostMort, 
                       newdata = newdataNonBlack,
                       type="link")
###########################################################
# Also get the probabilities
# Here you use type="response"
###########################################################
fHatBlack_prob = predict(fit2GLMBostMort,
                    newdata = newdataBlack,
                    type="response")
fHatNonBlack_prob = predict(fit2GLMBostMort, 
                       newdata = newdataNonBlack,
                       type="response")
###########################################################
# Now plot of logits
###########################################################
pdf("GLM.logits.of.denial.pdf")
plot(thenewDir,fHatBlack,col = "blue",xlab="Debt to Income Ratio",
     ylab="logit(Mortgage Denied)",
     type="l",lwd=2,ylim=c(-5,4),xlim=c(0,1.5),
     main = "Logits of Mortgane Denial")
lines(thenewDir,fHatNonBlack,col = "black",
      xlab="Debt to Income Ratio",type="l",lwd=2)
legend("topleft",c("Black","Non-Black"),lwd=2,col=c("blue","black"))
dev.off()

###########################################################
# Now plot the probits
###########################################################
pdf("GLM.probss.of.denial.pdf")
plot(thenewDir,fHatBlack_prob,col = "blue",
     xlab="Debt to Income Ratio",
     ylab="pr(Mortgage Denied)",
     type="l",lwd=2,ylim=c(0,1),xlim=c(0,1.5),
     main = "Probs of Denial")
lines(thenewDir,fHatNonBlack_prob,col = "black",
      xlab="Debt to Income Ratio",type="l",lwd=2)
legend("topleft",c("Black","Non-Black"),
       lwd=2,col=c("blue","black"),cex=0.75)
dev.off()

