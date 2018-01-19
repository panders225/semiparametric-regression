###########################################################
# This is the code needed to install all the packages
# except the HRW package. It only needs to be run once
#
# WARNING: There may be some backward noncompatibility,
# i.e. two different packages may have functions of the
# same name that do different things. The library command
# will overcome this issue
###########################################################
# Clear the workspace
rm(list = ls())
install.packages("mgcv", dependencies=TRUE)
install.packages("gam", dependencies=TRUE)
install.packages("gamlss", dependencies=TRUE)
install.packages("lme4", dependencies=TRUE)
install.packages("nlme", dependencies=TRUE)
install.packages("refund", dependencies=TRUE)
install.packages("fields", dependencies=TRUE)
install.packages("cosso", dependencies=TRUE)
install.packages("quantreg", dependencies=TRUE)
install.packages("fda", dependencies=TRUE)
install.packages("fda.usc", dependencies=TRUE)
install.packages("Ecdat", dependencies=TRUE)
install.packages("VGAM", dependencies=TRUE)

