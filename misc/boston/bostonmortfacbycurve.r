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
# This code creates a version of Figure 3.14 
# concerning a factor-by-curve interaction
# model for the Boston Mortgage data.
#
# Fit model:
##########################################################

fitInt = gam(deny ~ black + s(dir,by = factor(pbcr),k = 27)
                   + s(lvr,by = factor(pbcr),k = 27)
                   + pbcr + self + single + s(ccs,k = 4), 
                     family = binomial(link='logit'), 
                    data = BostonMortgages)
summary(fitInt)
##########################################################
# Set up grids for customised plots of smooth fits:
# 
# For covariates not a part of the possible interaction
# with pbcr, the fits are at their "yes" values or their
# mean values for continuous variables
##########################################################
ng = 1001
# Grid for dir
dirLow = 0 ; dirUpp = 1
dirg = seq(dirLow,dirUpp,length = ng)
# Grid for lvr
lvrLow = 0 ; lvrUpp = 1
lvrg = seq(lvrLow,lvrUpp,length = ng)
# Average of dir and lvr and ccs
dirAveg = rep(mean(BostonMortgages$dir),ng)
lvrAveg = rep(mean(BostonMortgages$lvr),ng)
ccsAveg    = rep(mean(BostonMortgages$ccs),ng)
# Yes and no factors for pbcr
pbcrNog  = as.factor(rep("no",ng))
pbcrYesg = as.factor(rep("yes",ng))
# Yes factors for the other dummy variables
blackYesg = as.factor(rep("yes",ng))
singleYesg = as.factor(rep("yes",ng))
selfYesg   = as.factor(rep("yes",ng))
# Set colors
nonPbcrCol = "indianred3"
PbcrCol    = "darkgreen"
##########################################################
# Set graphical parameters:
##########################################################
par(mfrow = c(1,2),mai = c(1.02,0.9,0.3,0.2)) ; 
cex.labVal = 1.4
##########################################################
# Get the predictiosn and pointwise dtandard errors
# as a function of "dir":
##########################################################
fdirNog = predict(fitInt,type = "response",
                   newdata = data.frame(self = selfYesg,dir = dirg,
                   lvr = lvrAveg,pbcr = pbcrNog,black = blackYesg,
                   single = singleYesg,ccs = ccsAveg),se = TRUE)
fdirYesg = predict(fitInt,type = "response",
                    newdata = data.frame(self = selfYesg,dir = dirg,
                    lvr = lvrAveg,pbcr = pbcrYesg,black = blackYesg,
                    single = singleYesg,ccs = ccsAveg),se = TRUE)
probdirNog = fdirNog$fit
sdprobNog  = fdirNog$se.fit
# Get the upper and lower limits for pbcr = no
# and makesure they cannot get outside 0 and 1
lowprobdirNog = probdirNog - 2*sdprobNog 
uppprobdirNog = probdirNog + 2*sdprobNog
lowprobdirNog[lowprobdirNog<0] = 0
uppprobdirNog[uppprobdirNog>1] = 1
# Get the fit and upper and lower limits for pbcr = yes
# and makesure they cannot get outside 0 and 1
probdirYesg = fdirYesg$fit
sdprobYesg = fdirYesg$se.fit
lowprobdirYesg = probdirYesg - 2*sdprobYesg 
uppprobdirYesg = probdirYesg + 2*sdprobYesg
lowprobdirYesg[lowprobdirYesg<0] = 0
uppprobdirYesg[uppprobdirYesg>1] = 1
# Set up the first plot as a function of dir
pdf('Boston_Interaction_Credit_CI.pdf')
plot(0,type = "n",xlim = range(dirg),ylim = c(0,1),
     xlab = "debt to income ratio",
     ylab = "probability of mortgage application denied",
     cex.lab = cex.labVal)
rug(BostonMortgages$dir,col = "dodgerblue",quiet = TRUE)
# Plot the lines and CI limits for pbcr = no
lines(dirg,probdirNog,col    = nonPbcrCol,lwd = 2)
lines(dirg,lowprobdirNog,col = nonPbcrCol,lwd = 2,lty = 2)
lines(dirg,uppprobdirNog,col = nonPbcrCol,lwd = 2,lty = 2)
# Plot the lines and CI limits for pbcr = yes
lines(dirg,probdirYesg,col = PbcrCol,lwd = 2)
lines(dirg,lowprobdirYesg,col = PbcrCol,lwd = 2,lty = 2)
lines(dirg,uppprobdirYesg,col = PbcrCol,lwd = 2,lty = 2)
# No idea what this is
abline(h = 0,col = "slateblue",lty = 2)
abline(h = 1,col = "slateblue",lty = 2)
# Put in a legend
legend(0.42,0.25,legend = c("Credit Issue","no Credit Issue"),
       lty = rep(1,2),lwd = rep(2,2),col = c(PbcrCol,nonPbcrCol),cex = 0.8)

# Do plot as a function of "lvr":
# Basically the same code, but with lvr instead of dir

flvrNog = predict(fitInt,type = "response",
                   newdata = data.frame(self = selfYesg,dir = dirAveg,
                   lvr = lvrg,pbcr = pbcrNog,black = blackYesg,
                   single = singleYesg,ccs = ccsAveg),se = TRUE)
flvrYesg = predict(fitInt,type = "response",
                    newdata = data.frame(self = selfYesg,dir = dirAveg,
                    lvr = lvrg,pbcr = pbcrYesg,black = blackYesg,
                    single = singleYesg,ccs = ccsAveg),se = TRUE)

problvrNog = flvrNog$fit
sdprobNog = flvrNog$se.fit

lowproblvrNog = problvrNog - 2*sdprobNog 
uppproblvrNog = problvrNog + 2*sdprobNog
lowproblvrNog[lowproblvrNog<0] = 0
uppproblvrNog[uppproblvrNog>1] = 1

problvrYesg = flvrYesg$fit
sdprobYesg = flvrYesg$se.fit
lowproblvrYesg = problvrYesg - 2*sdprobYesg 
uppproblvrYesg = problvrYesg + 2*sdprobYesg
lowproblvrYesg[lowproblvrYesg<0] = 0
uppproblvrYesg[uppproblvrYesg>1] = 1

plot(0,type = "n",xlim = range(lvrg),ylim = c(0,1),
     xlab = "loan size to property value ratio",
     ylab = "probability of mortgage application denied",
     cex.lab = cex.labVal)
rug(BostonMortgages$lvr,col = "dodgerblue",quiet = TRUE)

lines(lvrg,problvrNog,col = nonPbcrCol,lwd = 2)
lines(lvrg,lowproblvrNog,col = nonPbcrCol,lwd = 2,lty = 2)
lines(lvrg,uppproblvrNog,col = nonPbcrCol,lwd = 2,lty = 2)

lines(lvrg,problvrYesg,col = PbcrCol,lwd = 2)
lines(lvrg,lowproblvrYesg,col = PbcrCol,lwd = 2,lty = 2)
lines(lvrg,uppproblvrYesg,col = PbcrCol,lwd = 2,lty = 2)

abline(h = 0,col = "slateblue",lty = 2)
abline(h = 1,col = "slateblue",lty = 2)
dev.off()
###########################################################
# Get rid of the pointwise confidence intervals
###########################################################
pdf('Boston_Interaction_Credit_NoCI.pdf')
plot(0,type = "n",xlim = range(dirg),ylim = c(0,1),
     xlab = "debt to income ratio",
     ylab = "probability of mortgage application denied",
     cex.lab = cex.labVal)
rug(BostonMortgages$dir,col = "dodgerblue",quiet = TRUE)
lines(dirg,probdirNog,col    = nonPbcrCol,lwd = 2)
lines(dirg,probdirYesg,col = PbcrCol,lwd = 2)

legend(0.25,0.10,legend = c("Bad Credit","Good Credit"),
       lty = rep(1,2),lwd = rep(2,2),col = c(PbcrCol,nonPbcrCol),cex = 0.8)


plot(0,type = "n",xlim = range(lvrg),ylim = c(0,1),
     xlab = "loan size to property value ratio",
     ylab = "probability of mortgage application denied",
     cex.lab = cex.labVal)
rug(BostonMortgages$lvr,col = "dodgerblue",quiet = TRUE)
lines(lvrg,problvrNog,col = nonPbcrCol,lwd = 2)
lines(lvrg,problvrYesg,col = PbcrCol,lwd = 2)
legend(0.00,0.60,legend = c("Bad Credit","Good Credit"),
       lty = rep(1,2),lwd = rep(2,2),col = c(PbcrCol,nonPbcrCol),cex = 0.8)
dev.off()