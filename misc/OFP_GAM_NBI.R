########## R script: OFPGAM ##########

# For producing plot of the estimated smooth function
# components for the Poisson generalized additive model
# fit to the physician office visits data on both the
# link scale and the response scale.

# Last changed: 13 JUN 2017

# Load required packages:
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
setwd("C:\\Users\\Carroll\\Documents\\My_Documents\\2018_SemiPar\\Data_Sets\\OFP_Poisson")
################################################################################
# Get the necessary files
################################################################################
library(HRW)  
library(mgcv) 
library(Ecdat) 
################################################################################
# Get the Data
################################################################################
data(OFP) 
OFPforAna <- OFP
OFPforAna$age <- 10*OFPforAna$age
OFPforAna <- OFPforAna[OFPforAna$age <= 95,]
################################################################################
# Obtain generalized additive model with a NBI
################################################################################
fitGAMOFP_NBI = gam(ofp ~ s(age) + s(school) + adldiff +  black 
                 + sex + maried + privins + medicaid + region + hlth,
	         family = nb, data = OFPforAna)
################################################################################
# What is statistically significant?
################################################################################
summary(fitGAMOFP_NBI)
################################################################################
# Plot the estimated smooth function components fits on both 
# the link scale and the response scale: Use the pure Poisson
################################################################################
#pdf("Office_Visits_NBI.pdf")
par(mfrow = c(2,2),lwd = 2,cex.lab = 1,
    cex.main = 1,col.main = "navy",cex.axis = 1,
    mai = c(0.9,1.2,0.35,0.05))
# Get the first function in the link scale
plot(fitGAMOFP_NBI,select = 1,shade = TRUE,shade.col = "palegreen",
     main = "link, Negative Binomial",xlab = "age in years",ylab="Mean Visits",rug=FALSE)
rug(jitter(OFPforAna$age),col = "dodgerblue")
# Get the second function in the link scale
plot(fitGAMOFP_NBI,select = 2,shade = TRUE,shade.col = "palegreen",
     main = "link, Negative Binomial",xlab = "number of years of education",
     ylab = "Mean Visits",rug = FALSE)
rug(jitter(OFPforAna$school),col = "dodgerblue")
# Get the first function in the response scale
plot(fitGAMOFP_NBI,select = 1,shade = TRUE,shade.col = "palegreen",trans = exp,
     scale = FALSE,main = "response, Negative Binomial",xlab = "age in years",ylab = "Mean Visits",
     rug = FALSE)
rug(jitter(OFPforAna$age),col = "dodgerblue")
# Get the second function in the response scale
plot(fitGAMOFP_NBI,select = 2,shade = TRUE,shade.col = "palegreen",trans = exp,
     scale = FALSE,main = "response, Negative Binomial",xlab = "number of years of education",
     ylab = "",rug = FALSE)
rug(jitter(OFPforAna$school),col = "dodgerblue")
#dev.off()
############ End of OFPGAM ############

