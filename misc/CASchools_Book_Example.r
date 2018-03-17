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
#setwd("C:\\Users\\Carroll\\Documents\\My_Documents\\2018_Semi_in_R\\Data_Sets\\Boston_Housing")
setwd("C:\\Users\\Carroll\\Documents\\My_Documents\\2018_SemiPar\\Data_Sets\\COSSO_Examples")

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
# Ordinarily, you would now go back and run a gam with
# only the selected variables.
#
# In this code, I use REML to estimate the penalty
# and set k=20 for all smooths
###########################################################
library(mgcv)
Caschoolgam = gam(mathscr ~ s(calwpct,k=20) + s(log.avginc,k=20)
                          +  s(mealpct,k=20)+s(elpct,k=20),
                  family=gaussian, data = Caschool,
                  method="REML")
plot(Caschoolgam)


###########################################################
# This code shows that happens when you replace
# the numeric % English learned with the indicator
# that this is > 40%
###########################################################
zz = 1
if (zz > 0.5){
# Now redo but with the percentage of Engllish learners 
# replaced by the binary indicator that this perc4ntage is > 30
Caschool$highelpct = 1 * (Caschool$elpct > 30)
X_bin <- data.frame(Caschool$calwpct,Caschool$log.avginc,
                Caschool$mealpct,Caschool$highelpct,
                Caschool$expnstu,Caschool$str)
names(X_bin) <- c("percent qualifying for CalWorks",
              "log(average district income)",
              "percent qualifying for reduced-price lunch",
              "high percent English learners",
              "expenditure per student",
              "student:teacher ratio")
fitCOSSO_bin <- cosso(X_bin,mathScore,scale = TRUE,family=c("Gaussian"))
COSSOoutMat_bin <- data.frame(fitCOSSO_bin$tune$Mgrid,
                          fitCOSSO_bin$tune$L2norm)
names(COSSOoutMat_bin) <- c("M","%CalWks","log(inc.)","%lunch>30",
                        "%English","expend.","stud:teach")
print(round(COSSOoutMat_bin,1))
#pdf("Cosso_Pick_M_bin.pdf")
tuneCOSSO_bin <- tune.cosso(fitCOSSO_bin)
#dev.off()
print(tuneCOSSO_bin)
#pdf("COSSO_Fitted_Functions_bin.pdf")
plot.cosso(fitCOSSO_bin,M = tuneCOSSO_bin$OptM,plottype = "Func")
#dev.off()


library(mgcv)
Caschoolgam = gam(mathscr ~ s(calwpct,k=20) + s(log.avginc,k=20)
                  +  s(mealpct,k=20)+s(elpct,k=20),
                  family=gaussian, data = Caschool,
                  method="REML")
plot(Caschoolgam)

}