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
setwd("C:\\Users\\Carroll\\Documents\\My_Documents\\2018_Semi_in_R\\Data_Sets")
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
###########################################################
fit1GAMBostMort <- gam(deny ~ black + s(dir,bs="cr",k=20) 
                       + s(lvr,bs="cr",k=20) 
                       + pbcr + self + single + as.factor(ccs), 
                       family = binomial(link="logit"),
                       method="REML",
                       data = BostonMortgages)

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
#pdf("Boston_Mortgages_REML_k20.pdf")
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
     main = "REML, k=20, link scale",rug = FALSE)
rug(BostonMortgages$dir,col = "dodgerblue",quiet = TRUE)

###########################################################
# Now get the gam fit to lvr
###########################################################
plot(fit1GAMBostMort,shade = TRUE,shade.col = "palegreen", 
     xlim = c(0,1), select = 2, ylab = "Logit of Prob of denial", 
     xlab = "loan size to property value ratio",
     main = "REML, k=20, link scale",rug = FALSE)
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
     xlab = "debt payment to income ratio",
     main = "REML, k=20, response scale",rug = FALSE)
rug(BostonMortgages$dir,col = "dodgerblue",quiet = TRUE)

###########################################################
# Get the probabilities as a function of lvr
###########################################################
plot(fit1GAMBostMort,shade = TRUE,shade.col = "palegreen",
     trans = plogis,xlim = c(0,1),scale = FALSE,select = 2, 
     ylab = "Probability of denial", 
     xlab = "loan size to property value ratio",
     main = "REML, k=20, response scale",rug = FALSE)
rug(BostonMortgages$lvr,col = "dodgerblue",quiet = TRUE)

#dev.off()
############ End of BostMortGAMfit ############


