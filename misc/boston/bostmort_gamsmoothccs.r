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
setwd("C:\\Users\\Carroll\\Documents\\My_Documents\\2018_Semi_in_R\\Data_SetsBoston_Housing")
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
#
# I include the code where ccs was treated as a factor
#
# Then I fit the credit score as a smooth term
# but with only k=4
###########################################################
fit1GAMBostMort <- gam(deny ~ black + s(dir) + s(lvr) 
                      + pbcr + self + single + as.factor(ccs), 
                      method="REML",
                      family = binomial(link="logit"),
                      data = BostonMortgages)

fit2GAMBostMort <- gam(deny ~ black + s(dir) + s(lvr) 
                       + pbcr + self + single + s(ccs,k=4), 
                       method="REML",
                       family = binomial(link="logit"),
                       data = BostonMortgages)
###########################################################
# I obtain the summary to see if credit score, ccs, 
# remains statistically significant
###########################################################
summary(fit2GAMBostMort)
###########################################################
# Next I compare the two models. The as.factor model
# is more complex than the spline model, remember
###########################################################
anova(fit2GAMBostMort,fit1GAMBostMort,test="Chisq")
#pdf("Boston_Mortgages_Default.pdf")
###########################################################
# We can plot the smooth ccs plot as well 
###########################################################
par(mfrow = c(1,1),cex.lab = 1.5,
    cex.main = 1.5,col.main = "navy",lwd = 2,
    mai = c(0.9,0.7,0.35,0.05))

###########################################################
# This plots the spline in debt to oncome ratio.
# Since in the gam code, s(dit) comes first, the term
# select=1 means you are getting the gam fit to dir
###########################################################
# pdf('Boston_Effect_CreditScore.pdf')
plot(fit2GAMBostMort,shade = TRUE,shade.col = "palegreen", 
     select = 3,ylab = "Logit of Prob of Denial", 
     trans = plogis,
     xlab = "Credit Score",
     main = "Probability of denial",rug = FALSE)
dev.off()


###########################################################
# Plot the estimated GAM components:
###########################################################


###########################################################
# This sets up to plot things into one 2x2 plot
#
# The mai command gives the margin sizes in inches. I
# think this is to make the plot fit into the book
#
# The col.main controls the color of the figure titles,
# I think
###########################################################
#pdf("Boston_Mortgages_Default.pdf")
par(mfrow = c(2,2),cex.lab = 1.5,
     cex.main = 1.5,col.main = "navy",lwd = 2,
     mai = c(0.9,0.7,0.35,0.05))

###########################################################
# This plots the spline in debt to oncome ratio.
# Since in the gam code, s(dit) comes first, the term
# select=1 means you are getting the gam fit to dir
###########################################################
plot(fit1GAMBostMort,shade = TRUE,shade.col = "palegreen", 
     select = 1,xlim = c(0,1.5),ylab = "Logit of Prob of denial", 
     xlab = "Debt Payment to Income Ratio",
     main = "link scale",rug = FALSE)
rug(BostonMortgages$dir,col = "dodgerblue",quiet = TRUE)

###########################################################
# Now get the gam fit to lvr
###########################################################
plot(fit1GAMBostMort,shade = TRUE,shade.col = "palegreen", 
     xlim = c(0,1), select = 2, ylab = "Logit of Prob of denial", 
     xlab = "loan size to property value ratio",main = "link scale",rug = FALSE)
rug(BostonMortgages$lvr,col = "dodgerblue",quiet = TRUE)

###########################################################
# This gets the probability of denial for debt to
# income ratio. You still use the select command but
# the HRW package has a command trans which, if = plogis,
# gives the probabilities in the logistic regression
###########################################################
plot(fit1GAMBostMort,shade = TRUE,shade.col = "palegreen",
     trans = plogis,xlim = c(0,1.5),scale = FALSE,select = 1, 
     ylab = "Probability of Denial", 
     xlab = "debt payment to income ratio",main = "response scale",rug = FALSE)
rug(BostonMortgages$dir,col = "dodgerblue",quiet = TRUE)

###########################################################
# Get the probabilities as a function of lvr
###########################################################
plot(fit1GAMBostMort,shade = TRUE,shade.col = "palegreen",
     trans = plogis,xlim = c(0,1),scale = FALSE,select = 2, 
     ylab = "Probability of denial", 
     xlab = "loan size to property value ratio",main = "response scale",rug = FALSE)
rug(BostonMortgages$lvr,col = "dodgerblue",quiet = TRUE)


#dev.off()
############ End of BostMortGAMfit ############


