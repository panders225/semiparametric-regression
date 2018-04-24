###########################################################
# The Bivariate spline example in Section 5.2 of the
# HRW Book
###########################################################
###########################################################
# Clear the workspace. I do this every time
###########################################################
#rm(list = ls())
###########################################################
# Set the seed. I also do this every time
###########################################################
#set.seed(4428967)
###########################################################
# Set the working directory. 
###########################################################
#setwd("C:\\Users\\Carroll\\Documents\\My_Documents\\2018_SemiPar\\Data_Sets\\Bivariate_Splines")
###########################################################
# Set the libraries
###########################################################
library(HRW)
library(mgcv)
install.packages("fields")
library(fields) 
###########################################################
# Get the data, their names and their length
###########################################################
data(ozoneSub)
str(ozoneSub)
names(ozoneSub)
cat("The length of the Ozone data set = ",dim(ozoneSub)[1],"\n")
###########################################################
# Fit a default tensor product spline
###########################################################
fitOzoneTensProd = mgcv::gam(ozone ~ te(longitude,latitude),
                        data = ozoneSub,method = "REML")


summary(fitOzoneTensProd)
mgcv::gam.check(fitOzoneTensProd)
names(fitOzoneTensProd)
cat('Penalty in the longitude scale = ',fitOzoneTensProd$sp[1],"\n")
cat('Penalty in the  latitude scale = ',fitOzoneTensProd$sp[2],"\n")
fitOzoneTensProd$coefficients
cat("The regression variance = ",fitOzoneTensProd$sig2,"\n")
###########################################################
# Fit a normal additive model and test whether it is
# Sufficient
###########################################################
fitOzoneTensAdditive = mgcv::gam(ozone ~ s(longitude)+s(latitude),
                       data = ozoneSub,method = "REML")
qq = anova(fitOzoneTensAdditive,fitOzoneTensProd,test="Chisq")
cat("The p-value for the additive model = ",qq[2,5],"\n")
###########################################################
# Do a check to see if you have enough basis functions
###########################################################
gam.check(fitOzoneTensProd)
###########################################################
# Now see if more basis functions will be useful
###########################################################
fitOzoneTensProd10 = gam(ozone ~ te(longitude,latitude,k=10),
                       data = ozoneSub,method = "REML")
gam.check(fitOzoneTensProd10)
###########################################################
# Plot the function
###########################################################
#pdf("Ozone_Results_NoOzoneLevels.pdf")
par(mfrow=c(1,1))

test <- plot(fitOzoneTensProd10,scheme = 2,hcolors = terrain.colors(1000),
     main="",bty="l", cex.lab = 2,cex.axis = 2,
     xlab = "degrees longitude",
     ylab = "degrees latitude")

str(test)
# Plot the monioring stations
points(ozoneSub$longitude,ozoneSub$latitude,col = "dodgerblue")
# Outlines of the states
US(add = TRUE,lwd = 2)
# Names of the cities. You need to put in longitude and latitude
cityNames <- c("Chicago","Indianapolis","Milwaukee",
               "St Louis")
cityLonLatMat <- rbind(c(-87.6298,41.8781),
                       c(-86.1581,39.7694),
                       c(-87.9065,43.0389),
                       c(-90.1994,38.6270))
# Now print the city names
for (iCity in 1:length(cityNames))
{
  points(cityLonLatMat[iCity,1],cityLonLatMat[iCity,2],
         col = "navy",pch = 16)
  text(cityLonLatMat[iCity,1] + 0.15,cityLonLatMat[iCity,2],
       cityNames[iCity],adj = 0,cex = 1.8)
}
dev.off()
###########################################################
# This code does much the same thing except that it also
# tells you what the colors mean
###########################################################
# Get the grid
pdf("Ozone_Results_WithOzoneLevels.pdf")
par(mfrow=c(1,1))
ngrid <- 201
lonGrid <- seq(min(ozoneSub$longitude),
               max(ozoneSub$longitude),length = ngrid)
latGrid <- seq(min(ozoneSub$latitude),max(ozoneSub$latitude),
               length = ngrid)
lonlatMesh <- expand.grid(lonGrid,latGrid)
names(lonlatMesh) <- c("longitude","latitude")
# Obtain the fitted surface over the mesh:
fitMesh <- matrix(predict(fitOzoneTensProd10,
                          newdata = lonlatMesh),ngrid,ngrid)
par(mai = c(1.02,0.95,0.1,1.2)) 
library(fields)
image.plot(lonGrid,latGrid,fitMesh,col = terrain.colors(1000),
           xlab = "degrees longitude",ylab = "degrees latitude",
           legend.args = list(text = "ozone concentration",
                              cex = 1,adj = 0.8),axis.args = list(cex.axis = 1),
           xlim = c(-94.103,-82.429),
           ylim = c(36.408,44.836),bty = "l",
           cex.lab = 1,cex.axis = 1)
#lines(ozoneBdry,col = "navy")
points(ozoneSub$longitude,ozoneSub$latitude,
       col = "dodgerblue")
US(add = TRUE,lwd = 2)
cityNames = c("Chicago","Indianapolis","Milwaukee",
               "St Louis")
cityLonLatMat <- rbind(c(-87.6298,41.8781),
                       c(-86.1581,39.7694),
                       c(-87.9065,43.0389),
                       c(-90.1994,38.6270))
for (iCity in 1:length(cityNames))
{
  points(cityLonLatMat[iCity,1],cityLonLatMat[iCity,2],
         col = "navy",pch = 16)
  text(cityLonLatMat[iCity,1] + 0.15,cityLonLatMat[iCity,2],
       cityNames[iCity],adj = 0,cex = 1.8)
}
dev.off()