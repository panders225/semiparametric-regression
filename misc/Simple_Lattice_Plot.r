###########################################################
# Clear the workspace. I do this every time
###########################################################
rm(list = ls())
###########################################################
# Set the seed. I also do this every time
###########################################################
set.seed(4428967)
###########################################################
# Set the working directory. 
###########################################################
#setwd("C:\\Users\\Carroll\\Documents\\My_Documents\\2018_SemiPar\\Data_Sets\\Splnal_Bone_Mineral_Density")
###########################################################
# Look at the final plot
###########################################################
#pdf("femSBMDraw.pdf")
###########################################################
# Load the libraries and get the documentation
###########################################################
library(lattice)
library(HRW)
data(femSBMD)
help(xyplot)
###########################################################
# Run Wand Code
###########################################################
femSBMDvis <- xyplot(spnbmd ~ age|factor(ethnicity),
                     group = idnum,as.table = TRUE,
                     data = femSBMD,
                     strip = strip.custom(par.strip.text
                                          = list(cex = 1.5)),
                     par.settings = list(layout.heights
                                         =list(strip=1.6)),
                     scales = list(cex = 1.25),
                     xlab = list("age (years)",cex = 1.5),
                     ylab = list(expression(paste(
                       "spinal bone mineral density (g/c",m^2,")")),
                       cex = 1.5),
                     panel = function(x,y,subscripts,groups)
                     {  
                       panel.grid() 
                       panel.superpose(x,y,subscripts,groups,
                                       type = "b",pch = 16,lwd = 2)
                     })
plot(femSBMDvis)
dev.off()

