###########################################################
# Skeena River Sockeye Salmon Data
#
# Year is the year of measurement
# 
# Spawners is the number of Mother Fish
#
# Recruits is the Number of baby fish that escaped the river
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
setwd("C:\\Users\\Carroll\\Documents\\My_Documents\\2018_Semi_in_R\\Data_Sets\\Skeena_River")
###########################################################
# Get the data
###########################################################
thedata   = read.csv(file='Skeena_River.csv')
Year      = thedata$Year
Spawners  = thedata$Spawners
Recruits  = thedata$Recruits
plot(Spawners,Recruits,type="p")
