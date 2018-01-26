
##################
# Homework 3
# Philip Anderson
# STAT 689
##################

# third-party modules
library("data.table")

# read in data
fossil <- data.table::fread("C:/Users/Philip/Schools/TAMU/STAT_689/homework/hw03/fossil.csv")
names(fossil) <- tolower(names(fossil))


# take a look
head(fossil)
summary(fossil)
dim(fossil)


##################
# 2) Generate a scatterplot of the data
##################

plot(fossil$age, fossil$strontium.ratio
     , main="Strontium ratio vs. Age"
     , xlab="Fossil Age"
     , ylab="Fossil Strontium Ratio")
mtext("Fossil Data Set")


##################
# 3) Fit a linear model and add the fit to the scatterplot
##################

lin_mod <- lm(strontium.ratio ~ age
              , data=fossil)
abline(lin_mod, lwd=3, lty=2)


##################
# 4) Fit a quadratic model and add the fit to the scatterplot
##################

quad_mod <- lm(strontium.ratio ~ age + I(age^2)
               ,data=fossil)

x_vals <- seq(min(fossil$age), max(fossil$age), length=100)
quad_vals <- predict(quad_mod, newdata=data.frame(age=x_vals))
lines(x_vals, quad_vals, lwd=3, lty=3)

# this has not resulted in a dramatically better fit for the model

##################
# 5) Fit a cubic model and add the fit to the plot
##################

cub_mod <- lm(strontium.ratio ~ age + I(age^2) + I(age^3)
              , data=fossil
              )

cub_vals <- predict(cub_mod, newdata=data.frame(age=x_vals))
lines(x_vals, cub_vals, lwd=3, lty=4)

# the cubic model seems to have helped the cause
# it definitely fits the two primary curves of the line, but is still
# missing some points by quite a bit


##################
# 6) Fit a quartic model and add the fit to the scatterplot
##################

quart_mod <- lm(strontium.ratio ~ age + I(age^2) + I(age^3) + I(age^4)
                , data=fossil)

quart_vals <- predict(quart_mod, newdata=data.frame(age=x_vals))
lines(x_vals, quart_vals, lwd=3, lty=6)

# this did not result in a dramatic improvement over the cubic model

anova(cub_mod, quart_mod) 
# F-test has no significant difference
# use the relatively simpler, cubic model

legend(91, 0.70728
       , legend=c("Linear", "Quadratic", "Cubic", "Quartic")
       , lwd=rep(3,4)
       , lty=c(2, 3, 4, 6)
       )


