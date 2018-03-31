###################################################
# Get the library you need for analyzing 
# the Indiana Growth Curve Data
###################################################
library(nlme)   
###################################################
# Get the crucial variables
###################################################
age <- growthINblackMales$age
height <- growthINblackMales$height
idnum <-  growthINblackMales$idnum
###################################################
# Get ID numbers that are consecutive from 
# 1 to n. I have no idea why you need to do this
###################################################
idnumBM <- rep(NA,length(idnum))
uqID <- unique(idnum)
for (i in 1:length(uqID))
  idnumBM[idnum == uqID[i]] <- i
growthINblackMales$idnum <- idnumBM
