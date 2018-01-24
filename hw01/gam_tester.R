
library("mgcv")

dev.off()

plot(cars$speed, cars$dist, main="Speed x Distance")
lin_fit <- lm(dist ~ speed
              , data=cars)
abline(lin_fit, col="blue")



gam_fit <- mgcv::gam(dist ~ s(speed, bs="cr")
              , data=cars)
lines(cars$speed, fitted(gam_fit))

mgcv::gam.check(gam_fit)

##################
# try out gam on iris
##################

plot(iris$Sepal.Length, iris$Petal.Length
     , main="Petal Length by Sepal Length")

iris_gam <- mgcv::gam(Petal.Length ~ s(Sepal.Length)
                      , data=iris)
lines(iris$Sepal.Length, fitted(iris_gam))

mgcv::gam.check(iris_gam)

iris_gam
str(iris_gam)
iris_gam$aic
