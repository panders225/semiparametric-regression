###########################################################
# This is the Warsaw Apartment Data in a semiparametric
# ANCOVA without any Interactions
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
#setwd("C:\\Users\\Raymond\\Documents\\SemiPar_2018\\Data_Sets\\Warsaw.Data")
###########################################################
# Get the mgcv package
###########################################################
library(HRW)
library(mgcv)
###########################################################
# Get the Warsaw Data and name the crucial variables
###########################################################
data(WarsawApts)
thedate = WarsawApts$construction.date
areaPerMzloty = WarsawApts$areaPerMzloty
###########################################################
# Check out the various districts
###########################################################
district=WarsawApts$district
###########################################################
# The first district named is the default
# the other are compared to that district. 
# In this example, the reference district is Mokotow
###########################################################
cat('Here are the different districts',"\n")
cat('The first one named is the reference district',"\n")
unique(district)
###########################################################
# Set Srod as the reference district
###########################################################
district    = relevel(district, ref = "Srodmiescie")
###########################################################
# Run the model without an interaction, but with Srod as
# the reference
###########################################################
Srodancova = gam(areaPerMzloty~factor(district)
              +s(thedate,bs="cr",k=20),method="REML")
###########################################################
# Summarize the model
###########################################################
summary(Srodancova)
###########################################################
# Plot the lines and attach a legend
###########################################################
#pdf("Warsaw_Apartments_ANCOVA_NoInteract.pdf")
myLnCols=c(1,2,3,4)
plot(thedate,areaPerMzloty,type="n",
     xlab="construction date",
     ylab="area ratio",
     ylim = c(60,140))
ng = 1001
# District 1
construcDateg = seq(min(thedate),max(thedate),length = ng)
fHatgMokotow = predict(Srodancova, newdata = data.frame(
  thedate = construcDateg,
  district = rep("Mokotow",ng)))
# District 2
fHatgSrodmiescie = predict(Srodancova, newdata = data.frame(
  thedate = construcDateg,
  district = rep("Srodmiescie",ng)))
lines(construcDateg,fHatgSrodmiescie,col = myLnCols[2])
# District 3
fHatgWola = predict(Srodancova, 
                    newdata = data.frame(
                      thedate = construcDateg,
                      district = rep("Wola",ng)))
lines(construcDateg,fHatgWola,col = myLnCols[3])
# District 4
fHatgZoliborz = predict(Srodancova, 
                        newdata = data.frame(
                          thedate = construcDateg,
                          district = rep("Zoliborz",ng)))
lines(construcDateg,fHatgZoliborz,col = myLnCols[4])
# Legend
legend(1941,80,legend = c("Mokotow","Srodmiescie","Wola","Zoliborz"),
       col = c(1,2,3,4),lty=rep(1,4),cex=0.7)
#dev.off()