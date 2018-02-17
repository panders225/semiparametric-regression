################################################################################
# This is the Boston Housing Data, with a simple
# ANOVA without an interaction
#
# There are many possible confounders, but in this one
# I simply use Z = credit score
# 
# The first part of the code is the same as Simple_GLM.R
################################################################################
# Clear the workspace
rm(list = ls())
################################################################################
# Set the seed
################################################################################
set.seed(4428967)
################################################################################
# Set the working directory
################################################################################
setwd("C:\\Users\\Raymond\\Documents\\SemiPar_2018\\Data_Sets\\Warsaw.Data")
################################################################################
# Load libaries and data set
################################################################################
library(HRW)
library(mgcv)
data(BostonMortgages)
################################################################################
# Use only those data with dit < 1.5
################################################################################
NewBostonMortgages = BostonMortgages[BostonMortgages$dir < 1,]
dir = NewBostonMortgages$dir

################################################################################
# Fit ordinary logistic regression (no splines).
# ANCOVA without an interaction
################################################################################
fit2GLMBostMort <- glm(deny ~ black+dir,
                       family = binomial(link=logit),
                       data = NewBostonMortgages)
################################################################################
# For predictions: make a vector of closely-spaced points from dir=0 to dir=1
# with race black and another with race not-black
################################################################################
ng = 1001
thenewDir = seq(min(dir),max(dir),length = ng)
newdataBlack = data.frame(dir=thenewDir,black="yes")
newdataNonBlack = data.frame(dir=thenewDir,black="no")
################################################################################
# Predict logit(mortgage denied) for black and non-black clients
################################################################################
fHatBlackGLM = predict(fit2GLMBostMort,
                       newdata = newdataBlack,type="link")
fHatNonBlackGLM = predict(fit2GLMBostMort, 
                          newdata = newdataNonBlack)
################################################################################
# Plot of logistic regression model for black clients with a blue line.
# No splines on dir result in a linear fit to logit(mortgage denied).
# Restrict plot to dir < 1.5
################################################################################
# pdf('GLM_fit_BostonMortgage.pdf')
plot(thenewDir,fHatBlackGLM,col = "blue",
     xlab="dir",ylab="logit(mortgage denied)",
     type="l",lwd=2,ylim=c(-5,4),xlim=c(0,1.5),
     main="Logistic Regression: ANCOVA without interaction")
################################################################################
# Add a red line for non-black borrowers, 
# and add a legend in the top-left
################################################################################
lines(thenewDir,fHatNonBlackGLM,
      col = "black",xlab="dir",type="l",lwd=2)
legend("topleft",c("Black","Non-Black"),
       lwd=2,col=c("blue","red"))
################################################################################
# Add a "rug" of tick-marks indicating observed values of dir
################################################################################
rug(NewBostonMortgages$dir[NewBostonMortgages$black=="no"],
    col = "black")
rug(NewBostonMortgages$dir[NewBostonMortgages$black=="yes"],
    col = "blue")
# dev.off()

################################################################################
# Fit GAM (using mgcv) with spline smoothing dir
# This is ANCOVA without an interaction
################################################################################
fit2GAMBostMort <- gam(deny ~ black + s(dir),
                       family = binomial(link=logit),
                       data = NewBostonMortgages)
################################################################################
# Predict logit(mortgage denied) for black and non-black clients
# The type="link" says logits
################################################################################
fHatBlackGAM = predict(fit2GAMBostMort,
                       newdata = newdataBlack,)
fHatNonBlackGAM = predict(fit2GAMBostMort, 
                          type="link", newdata = newdataNonBlack)
################################################################################
# Plot of spline fit for black clients with a blue line, 
# and non-blacks with
# a black line. (Restrict plot to dir<1..5)
################################################################################
# pdf('GAM_fit_BostonMortgage.pdf')
plot(thenewDir,fHatBlackGAM,col = "blue",xlab="dir",
     ylab="logit(mortgage denied)",
     type="l",lwd=2,ylim=c(-3,2),xlim=c(0,1.5),
     main="GAM: deny vs. s(dir) for blacks and non-blacks")
lines(thenewDir,fHatNonBlackGAM,col = "black",
      xlab="dir",type="l",lwd=2)
################################################################################
# Add legend and "rug" of tick-marks indicating observed values of dir
################################################################################
legend("topleft",c("Black","Non-black"),lwd=2,col=c("blue","black"))
rug(NewBostonMortgages$dir[NewBostonMortgages$black=="no"],
    col = "black")
rug(NewBostonMortgages$dir[NewBostonMortgages$black=="yes"],
    col = "blue")
# dev.off()




if(!exists("opar")) opar <- par(no.readonly = TRUE)
# par(opar)

#############################################################
# Chapter 3
#############################################################
library(HRW) ; data(BostonMortgages)
denyBin <- as.numeric(BostonMortgages$deny == "yes")
blackIndic <- BostonMortgages$black == "yes"
print(round(mean(denyBin),2))
probabBlack <- mean(denyBin[blackIndic])
probabNotBlack <- mean(denyBin[!blackIndic])
oddsBlack <- probabBlack/(1 - probabBlack)
oddsNotBlack <- probabNotBlack /(1 - probabNotBlack)
print(round(oddsBlack/oddsNotBlack,1))

fit1GLMBostMort <- glm(deny ~ black,family = binomial,
                       data = BostonMortgages)
summary(fit1GLMBostMort)

fit2GLMBostMort <- glm(deny ~ black + dir + lvr + pbcr
                       + self + single + as.factor(ccs),
                       family = binomial,data = BostonMortgages)
summary(fit2GLMBostMort)

library(mgcv)
fit2GLMBostMortAlt <- gam(deny ~ black + dir + lvr + pbcr
                          + self + single + as.factor(ccs),
                          family = binomial,data = BostonMortgages)
summary(fit2GLMBostMortAlt)
fit1GAMBostMort <- gam(deny ~ black + s(dir) + s(lvr)
                       + pbcr + self + single + as.factor(ccs),family = binomial,
                       data = BostonMortgages)
summary(fit1GAMBostMort)

# demo(BostMortGAMfit,package = "HRW")

par(mfrow = c(2, 2), bty = "l", cex.lab = 1.5,
    cex.main = 1.5,col.main = "navy", lwd = 2,
    mai = c(0.9,0.7,0.35,0.05))

plot(fit1GAMBostMort, shade = TRUE, shade.col = "palegreen",
     select = 1, xlim = c(0,1), ylab = "effect of logit(probab. of denial)",
     xlab = "debt payment to income ratio",
     main = "link scale",rug = FALSE)
rug(BostonMortgages$dir,col = "dodgerblue")

plot(fit1GAMBostMort,shade = TRUE, shade.col = "palegreen",
     xlim = c(0, 1),select = 2, ylab = "effect on logit(probab. of denial)",
     xlab = "loan size to property value ratio",main = "link scale",rug = FALSE)
rug(BostonMortgages$lvr,col = "dodgerblue")

plot(fit1GAMBostMort, shade = TRUE, shade.col = "palegreen",
     trans = plogis,scale = FALSE,xlim = c(0,1),select = 1,
     ylab = "effect on probability of denial",
     xlab = "debt payment to income ratio",main = "response scale",rug = FALSE)
rug(BostonMortgages$dir,col = "dodgerblue")

plot(fit1GAMBostMort, shade = TRUE, shade.col = "palegreen",
     trans = plogis,scale = FALSE,xlim = c(0,1),select = 2,
     ylab = "effect on probability of denial",
     xlab = "loan size to property value ratio",main = "response scale", rug = FALSE)
rug(BostonMortgages$lvr,col = "dodgerblue")

anova(fit2GLMBostMort,fit1GAMBostMort,test = "Chisq")

fit2GAMBostMort <- gam(deny ~ black + s(dir) + s(lvr)
                       + pbcr + self + single + s(ccs,k = 4),family = binomial,
                       data = BostonMortgages)
summary(fit2GAMBostMort)

anova(fit2GAMBostMort,fit1GAMBostMort,test = "Chisq")

par(mfrow = c(1,1))
plot(fit2GAMBostMort, select = 3,shade = TRUE,shade.col = "palegreen",
     trans = plogis,scale = FALSE,bty = "l", lwd = 2,
     cex.lab = 1.5, cex.axis = 1.5, ylim = c(-0.6,1.5),
     xlab = "credit score", ylab = "effect on probability of denial",
     rug = FALSE)
rug(jitter(BostonMortgages$ccs),col="dodgerblue")

fitFacByCurvBostMort <- gam(deny ~ black
                            + s(dir,by = factor(self))
                            + s(lvr,by = factor(self)) +
                              + pbcr + self + single + s(ccs,k = 4),
                            family = binomial,data = BostonMortgages)
summary(fitFacByCurvBostMort)

anova(fit2GAMBostMort,fitFacByCurvBostMort,test = "Chisq")
































