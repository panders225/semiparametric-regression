

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
# with a p-value of 0.00216

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

# The log-log regression still has evidence of heteroscedacticity present
# the residuals demonstrate higher variance as we move to higher levels
# of fitted values

##################
# 5) Plot the fitted line and show the polygon overlay
##################

ord <- order(log(skeena$spawners))
x <- skeena$spawners[ord] %>% log()
y <- skeena$recruits[ord] %>% log()
pred_vec <- skeena$spawners

pred <- predict(lin_mod2
                , newdata=data.frame(spawners=pred_vec)
                , se.fit=TRUE
                )


# using n-2 df because we have n obs with 2 estimated parameters
t <- qt(0.975, df=n-2)
upperCI <- pred$fit[ord] + (t * pred$se.fit[ord])
lowerCI <- pred$fit[ord] - (t * pred$se.fit[ord])

# finally make our plot
plot(1, type="n"
     , xlim=c(min(x), max(x))
     , ylim=c(min(y), max(y))
     , xlab="Log(Spawners)"
     , ylab="Log(Recruits)"
     , main="Skeena Recruits vs. Spawners"
    )
mtext("Logarithmic Transformations Applied")

polygon(x=c(x, rev(x))
        , y=c(upperCI, rev(lowerCI))
        , col='gray'
        , border=NA
        )

lines(x, pred$fit[ord]
      , type='l'
      , col='black'
      , lwd=3
      )

