

##################
# HW02 - Philip Anderson
# STAT 689
##################

# third-party modules

install.packages("C:/Users/Philip/Schools/TAMU/STAT_689/homework/HRW_1.01.09.tar.gz", repos=NULL, type="source")
library("HRW")
library("data.table")
library("dplyr")


# read in the data
skeena <- data.table::fread("C:/Users/Philip/Schools/TAMU/STAT_689/homework/hw02/Skeena_River.csv")
names(skeena) <- tolower(names(skeena))

# take a look
head(skeena)
summary(skeena)

##################
# 1) Perform a linear regression of Y on X
# - do the residuals show heteroskedacticity?
##################
# Y = recruits
# X = spawners

lin_mod <- lm(recruits ~ spawners
              , data=skeena 
              )

par(mfrow=c(2,2))
plot(lin_mod)
par(mfrow=c(1,1))

# we will have hetereoskedacticity present if the plot of Residuals
# against fitted values show non-constant variance

# it appears that as we move along the fitted values of X, we have 
# an increase in the variability of Y, which gives evidence of heteroskedacticity

##################
# 2) Is there a statistically significant relationship between Y and X?
##################

summary(lin_mod)

# we have a statistically significant relationship between Y and X

plot(skeena$spawners, skeena$recruits)
abline(lin_mod)
# plot suggests mostly linear relationship

##################
# 3) Regress log(Y) on log(X)
# which points are obviously 1951 and 1955?
##################

lin_mod2 <- lm(log(recruits) ~ log(spawners)
               , data=skeena
               )

summary(lin_mod2)

# diagnostic plots
par(mfrow=c(2,2))
plot(lin_mod2)
par(mfrow=c(1,1))

skeena
summary(skeena)
# point 12 (1951) has an unusually low number of recruits (Y)
# point 16 (1955) has an unusually low number of spawners (X)

# point 12 will dramatically over-estimate the number of recruits, for a 
# low residual value, but high standardized residual value

# point 16 will have a very high leverage value.  This is evident from 
# the high Cook's distance value

##################
# 4) Does the log-log regression show heteroskedacticity?
##################

# The log-log regression has substantially reduced the evidence of 
# hetereoskedacticity.  The variance of the residuals slightly increases
# as we move along the fitted values, but it is not as dramatic as in the 
# non-transformed regression

##################
# 5) Plot the fitted line and show the polygon overlay
##################
help(polygon)

plot(log(skeena$spawners), log(skeena$recruits)
     , main="Transformed Recruits vs. Spawners"
     , xlab="ln(spawners)"
     , ylab="ln(recruits)"
     )
mtext("Linear Regression Overlay")
abline(lin_mod2)

newx <- seq(min(log(skeena$spawners)), max(log(skeena$spawners)), length.out=100)
preds <- predict(lin_mod2, newdata=data.frame(spawners=newx)
                 , interval="confidence")

plot(log(skeena$recruits) ~ log(skeena$spawners), type='n')
polygon(c(rev(newx), newx), c(rev(preds[,3]), preds[,2]), col='grey80', border=NA)
abline(lin_mod2)
lines(newx, preds[ ,3], lty = 'dashed', col = 'red')
lines(newx, preds[ ,2], lty = 'dashed', col = 'red')




