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
# Fit ordinary logistic regression (no splines)
# This is a simple analysis of covariance without interaction
################################################################################
fit2GLMBostMort <- glm(deny ~ black+dir, 
                       family = binomial(link=logit),
                       data = NewBostonMortgages)
################################################################################
# For predictions: make a vector of closely-spaced points 
# from dir=0 to dir=1.5
# with race black and another with race not-black
################################################################################
ng = 1001
thenewDir = seq(min(dir),max(dir),length = ng)
newdataBlack = data.frame(dir=thenewDir,black="yes")
newdataNonBlack = data.frame(dir=thenewDir,black="no")
################################################################################
# Predict logit(mortgage denied) for black and non-black clients.
# Get SE of prediction with "se.fit=TRUE".
#
# Notice the type="link" command is to get the logits
################################################################################
fHatBlackGLM_CI = predict(fit2GLMBostMort,newdata = newdataBlack,
                          type="link", se.fit=TRUE)
fHatNonBlackGLM_CI = predict(fit2GLMBostMort, newdata = newdataNonBlack,
                             type="link", se.fit=TRUE)
################################################################################
# Calculate upper & lower 95% CIs for black and non-black clients.
################################################################################
# For black
fHatBlackGLM = fHatBlackGLM_CI$fit
upCIfHatBlackGLM = fHatBlackGLM + 2*fHatBlackGLM_CI$se.fit
lowCIfHatBlackGLM = fHatBlackGLM - 2*fHatBlackGLM_CI$se.fit
# For non-Black
fHatNonBlackGLM = fHatNonBlackGLM_CI$fit
upCIfHatNonBlackGLM = fHatNonBlackGLM + 2*fHatNonBlackGLM_CI$se.fit
lowCIfHatNonBlackGLM = fHatNonBlackGLM - 2*fHatNonBlackGLM_CI$se.fit
################################################################################
# Plot the logistic regression model with 95% CI for 
# both black and non-black clients. Start by drawing a 
# blank plot with title & axis labels (restrict plot to dir < 1.5).
################################################################################
#pdf('GLM_fit_BostonMortgage_CI_RJC.pdf')
plot(0, col = "blue", xlab="Debt to Income Ratio", 
     ylab="logit(mortgage denied)",
     type="l", lwd=2, ylim=c(-5,4), xlim=c(0,max(dir)),
     main="ANCOVA: Logistic Regression")
################################################################################
# Shade the confidence intervals: semitransparent blue for black borrowers,
# and semitransparent red for non-black borrowers.
################################################################################
polygon(x=c(thenewDir, rev(thenewDir)),
        y=c(upCIfHatBlackGLM, rev(lowCIfHatBlackGLM)),
        col="pink")
polygon(x=c(thenewDir, rev(thenewDir)),
        y=c(upCIfHatNonBlackGLM, rev(lowCIfHatNonBlackGLM)),
        col="gray")
################################################################################
# Add a blue line for black borrowers, 
# and a black line for non-black borrowers
################################################################################
lines(thenewDir,fHatBlackGLM,
      col = "blue",xlab="Debt to Income Ratio",type="l",lwd=2)
lines(thenewDir,fHatNonBlackGLM,
      col = "black",xlab="Debt to Income Ratio",type="l",lwd=2)
################################################################################
# Add a legend in the top left corner
################################################################################
legend("topleft",c("Black","Non-black"),lwd=2,col=c("blue","black"))
################################################################################
# Add a "rug" of tick-marks indicating observed values of dir
################################################################################
rug(NewBostonMortgages$dir[NewBostonMortgages$black=="no"], 
    col = "black")
rug(NewBostonMortgages$dir[NewBostonMortgages$black=="yes"], 
    col = "blue")
#dev.off()

################################################################################
# Fit GAM (using mgcv) with smoothed dir
# The number of basis functions is the default, GCV is the default
################################################################################
fit2GAMBostMort <- gam(deny ~ black + s(dir), 
                       family = binomial(link=logit),
                       data = NewBostonMortgages)
################################################################################
# Predict logit(mortgage denied) for black and non-black clients.
# Get SE of prediction with "se.fit=TRUE".
#
# Again, the type="link" means the logits
################################################################################
fHatBlackGAM_CI = predict(fit2GAMBostMort,
                    newdata = newdataBlack,
                    type="link", se.fit=TRUE)
fHatNonBlackGAM_CI = predict(fit2GAMBostMort, 
                        newdata = newdataNonBlack, 
                        type="link", se.fit=TRUE)
################################################################################
# Calculate upper & lower 95% CIs for black and non-black clients.
################################################################################
# Blacks
fHatBlackGAM = fHatBlackGAM_CI$fit
upCIfHatBlackGAM = fHatBlackGAM + 2*fHatBlackGAM_CI$se.fit
lowCIfHatBlackGAM = fHatBlackGAM - 2*fHatBlackGAM_CI$se.fit
# Non-Blacks
fHatNonBlackGAM = fHatNonBlackGAM_CI$fit
upCIfHatNonBlackGAM = fHatNonBlackGAM + 2*fHatNonBlackGAM_CI$se.fit
lowCIfHatNonBlackGAM = fHatNonBlackGAM - 2*fHatNonBlackGAM_CI$se.fit
################################################################################
# Plot the GAM-spline model with 95% CI 
# for both black and non-black clients.
# Start by drawing a blank plot with 
# title & axis labels (restrict plot to dir < 1.5).
################################################################################
# pdf('GAM_fit_BostonMortgage_CI_RJC.pdf')
plot(0, col = "blue",xlab="Debt to Income Ratio", ylab="logit(mortgage denied)",
     type="l",lwd=2,ylim=c(-3,2),xlim=c(0,1.5),
     main="GAM: ANCOVA no interaction ")
################################################################################
# Shade the confidence intervals: 
# semitransparent blue for black borrowers,
# and semitransparent black for non-black borrowers.
################################################################################
polygon(x=c(thenewDir, rev(thenewDir)),
        y=c(upCIfHatBlackGAM, rev(lowCIfHatBlackGAM)),
        col="pink")
polygon(x=c(thenewDir, rev(thenewDir)),
        y=c(upCIfHatNonBlackGAM, rev(lowCIfHatNonBlackGAM)),
        col="lightgray")
################################################################################
# Add a blue line for black borrowers, and a red line for non-black borrowers
################################################################################
lines(thenewDir,fHatBlackGAM,col = "blue", 
      type="l", lwd=2)
lines(thenewDir,fHatNonBlackGAM,col = "black", type="l", lwd=2)
################################################################################
# Add legend and "rug" of tick-marks indicating observed values of dir
################################################################################
legend("topleft",c("Black","Non-black"),lwd=2,col=c("blue","red"))
rug(NewBostonMortgages$dir[NewBostonMortgages$black=="no"],
    col = "black")
rug(NewBostonMortgages$dir[NewBostonMortgages$black=="yes"],
    col = "blue")
# dev.off()

################################################################################
# Test whether the spline is needed
################################################################################
anova(fit2GLMBostMort,fit2GAMBostMort,test=Chisq)
