###########################################################
# This program uses the California School Data in the 
# original scale and sees if cosso and mgcv select the same
# variables
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
# Set the working directory. I probably do not need to 
# do this (it will not work in Linux), but I do to keep
# my blood pressure down
###########################################################
setwd("C:\\Users\\kravitze\\Desktop\\STAT689\\Lectures\\CA_Schools")
###########################################################
# Get libraries and data
###########################################################
library(cosso)
library(Ecdat); 
data(Caschool)
###########################################################
# Log the average income
###########################################################
Caschool$log.avginc <- log(Caschool$avginc)
###########################################################
# Get the response
###########################################################
mathScore <- Caschool$mathscr
###########################################################
# Set up the predictors and name then for output
###########################################################
X <- data.frame(Caschool$calwpct,Caschool$log.avginc,
                Caschool$mealpct,Caschool$elpct,
                Caschool$expnstu,Caschool$str)
names(X) <- c("percent qualifying for CalWorks",
              "log(average district income)",
              "percent qualifying for reduced-price lunch",
              "percent English learners",
              "expenditure per student",
              "student:teacher ratio")

###########################################################
# Run the cosso
###########################################################
fitCOSSO <- cosso(X,mathScore,scale = TRUE,family=c("Gaussian"))
###########################################################
# Show for what values of M that the variables enter
###########################################################
COSSOoutMat <- data.frame(fitCOSSO$tune$Mgrid,
                      fitCOSSO$tune$L2norm)
names(COSSOoutMat) <- c("M","%CalWks","log(inc.)","%lunch",
                        "%English","expend.","stud:teach")
print(round(COSSOoutMat,1))
###########################################################
# Now estimate what the best values of M is
###########################################################
# First plot to see what the best value of M is
#pdf("Cosso_Pick_M.pdf")
tuneCOSSO <- tune.cosso(fitCOSSO) ; 
#dev.off()
print(tuneCOSSO)
# Now show the fitted functions
#pdf("COSSO_Fitted_Functions.pdf")
plot.cosso(fitCOSSO,M = tuneCOSSO$OptM,plottype = "Func")
#dev.off()
###########################################################
# Let us see what the select=TRUE option in mgcv does with this example
###########################################################
library(mgcv)
Caschoolgam = gam(mathscr ~ s(calwpct,k=20) + s(log.avginc,k=20)
                          +  s(mealpct,k=20)+s(elpct,k=20)
                  + Caschool$expnstu + Caschool$str,
                  family=gaussian, data = Caschool,
                  method="REML",select=TRUE)
summary(Caschoolgam)
