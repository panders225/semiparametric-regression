###########################################################
# This is the code needed to run the demo on page 105
###########################################################
# Clear the workspace
rm(list = ls())
###########################################################
# Set the seed
###########################################################
set.seed(4428967)
###########################################################
# Set the working directory to the folder with HRW_1.01.09.tar.gz
###########################################################
setwd("C:/Users/Philip/Schools/TAMU/STAT_689/homework/")
phil_dir <- "C:/Users/Philip/Schools/TAMU/STAT_689/homework/hw01/" 
###########################################################
# Install the necessary packages
# This only needs to be done once
###########################################################
install.packages("HRW_1.01.09.tar.gz", repos=NULL, type="source")
install.packages('VGAM', dependencies=TRUE)
install.packages("Ecdat", dependencies=TRUE)
###########################################################
# Make the necessary packages active
###########################################################
library(HRW)
library(VGAM)
library(Ecdat)
###########################################################
# Find the file where the demo code is saved
###########################################################
system.file("demo","CaSchoolVGAMfit.R",package = "HRW")
###########################################################
# Now run the demo, and save the plot as a PDF
###########################################################
pdf(paste0(phil_dir, "Homework_01.pdf"))
demo(CaSchoolVGAMfit,package = "HRW")
dev.off()
###########################################################
# Submit this PDF as your first homework assignment
###########################################################
