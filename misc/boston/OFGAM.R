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
# Obtain a GLM fit to the variables
################################################################################
fitGLMOFP_Poisson = gam(ofp ~ age + school + adldiff +  black 
                        + sex + maried + privins + medicaid + region + hlth,
                        family = poisson, data = OFPforAna)
################################################################################
# Obtain generalized additive model with a pure Poisson
################################################################################
fitGAMOFP_Poisson = gam(ofp ~ s(age) + s(school) + adldiff +  black 
                 + sex + maried + privins + medicaid + region + hlth,
	         family = poisson, data = OFPforAna)
################################################################################
# Test for whether the spline is necessary
################################################################################
anova(fitGLMOFP_Poisson,fitGAMOFP_Poisson,test="Chisq")
################################################################################
# What is statistically significant?
################################################################################
summary(fitGAMOFP_Poisson)
################################################################################
# Obtain generalized additive model with estimated scale
################################################################################
fitGAMOFP_quasiPoisson = gam(ofp ~ s(age) + s(school) + adldiff +  black 
                        + sex + maried + privins + medicaid + region + hlth,
                        family = poisson, scale = -1, data = OFPforAna)
summary(fitGAMOverOFP)
################################################################################
# Test for Overdispersion
################################################################################
anova(fitGAMOFP_Poisson,fitGAMOFP_quasiPoisson,test="Chisq")


################################################################################
# Plot the estimated smooth function components fits on both 
# the link scale and the response scale: Use the quasi Poisson
################################################################################
#pdf("Office_Visits_quasiPoisson.pdf")
par(mfrow = c(2,2),lwd = 2,cex.lab = 1,
    cex.main = 1,col.main = "navy",cex.axis = 1,
    mai = c(0.9,1.2,0.35,0.05))
# Get the first function in the link scale
plot(fitGAMOFP_quasiPoisson,select = 1,shade = TRUE,shade.col = "palegreen",
     main = "link, scale estimated",xlab = "age in years",ylab="Mean Visits",rug=FALSE)
rug(jitter(OFPforAna$age),col = "dodgerblue")
# Get the second function in the link scale
plot(fitGAMOFP_quasiPoisson,select = 2,shade = TRUE,shade.col = "palegreen",
     main = "link, scale estimated",xlab = "number of years of education",
     ylab = "Mean Visits",rug = FALSE)
rug(jitter(OFPforAna$school),col = "dodgerblue")
# Get the first function in the response scale
plot(fitGAMOFP_quasiPoisson,select = 1,shade = TRUE,shade.col = "palegreen",trans = exp,
     scale = FALSE,main = "response, scale estimated",xlab = "age in years",ylab = "Mean Visits",
     rug = FALSE)
rug(jitter(OFPforAna$age),col = "dodgerblue")
# Get the second function in the response scale
plot(fitGAMOFP_quasiPoisson,select = 2,shade = TRUE,shade.col = "palegreen",trans = exp,
     scale = FALSE,main = "response, scale estimated",xlab = "number of years of education",
     ylab = "",rug = FALSE)
rug(jitter(OFPforAna$school),col = "dodgerblue")
#dev.off()
############ End of OFPGAM ############

################################################################################
# Plot the estimated smooth function components fits on both 
# the link scale and the response scale: Use the pure Poisson
################################################################################
#pdf("Office_Visits_PurePoisson.pdf")
par(mfrow = c(2,2),lwd = 2,cex.lab = 1,
    cex.main = 1,col.main = "navy",cex.axis = 1,
    mai = c(0.9,1.2,0.35,0.05))
# Get the first function in the link scale
plot(fitGAMOFP_Poisson,select = 1,shade = TRUE,shade.col = "palegreen",
     main = "link, Poisson",xlab = "age in years",ylab="Mean Visits",rug=FALSE)
rug(jitter(OFPforAna$age),col = "dodgerblue")
# Get the second function in the link scale
plot(fitGAMOFP_Poisson,select = 2,shade = TRUE,shade.col = "palegreen",
     main = "link, Poisson",xlab = "number of years of education",
     ylab = "Mean Visits",rug = FALSE)
rug(jitter(OFPforAna$school),col = "dodgerblue")
# Get the first function in the response scale
plot(fitGAMOFP_Poisson,select = 1,shade = TRUE,shade.col = "palegreen",trans = exp,
     scale = FALSE,main = "response, Poisson",xlab = "age in years",ylab = "Mean Visits",
     rug = FALSE)
rug(jitter(OFPforAna$age),col = "dodgerblue")
# Get the second function in the response scale
plot(fitGAMOFP_Poisson,select = 2,shade = TRUE,shade.col = "palegreen",trans = exp,
     scale = FALSE,main = "response, Poisson",xlab = "number of years of education",
     ylab = "",rug = FALSE)
rug(jitter(OFPforAna$school),col = "dodgerblue")
#dev.off()
############ End of OFPGAM ############

