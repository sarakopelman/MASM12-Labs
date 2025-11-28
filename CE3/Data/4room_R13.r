library(ctsmr)
library(splines)
source("CE3/Data/sde4room_R13.r") # change to correct model
load("CE3/Data/Exercise3.RData")
Hour <- as.numeric(strftime(AllDat$date, format="%H"))

idx <- (Hour>8 & Hour < 23) # It is impossible to fit a window area for the hours without any sun, so we limit the window area estimation to the hours with sun.

fit1 <- sde4room_R13(AllDat) # change to correct model

summary(fit1)
summary(fit1, extended=TRUE)

Pred1 <- predict(fit1)
res1 <- Pred1[[1]]$state$pred$Ti1 - AllDat$yTi1
res2 <- Pred1[[1]]$state$pred$Ti2 - AllDat$yTi2
res3 <- Pred1[[1]]$state$pred$Ti3 - AllDat$yTi3
res4 <- Pred1[[1]]$state$pred$Ti4 - AllDat$yTi4
AllDat$res1 <- res1
AllDat$res2 <- res2
AllDat$res3 <- res3
AllDat$res4 <- res4


plot(Hour,AllDat$res1, 
     col  = "blue",
     xlab = "Hour",
     ylab = "Residual",
     main = "Residuals over time")

points(Hour, res2,
       col  = "red")

points(Hour, res3,
       col  = "green")

points(Hour, res4,
       col  = "orange")

legend("bottomleft",
       legend = c("Ti1", "Ti2", "Ti3", "Ti4"),
       col    = c("blue", "red", "green", "orange"),
       lty    = 1,
       bty    = "n")

#rmse1 <- sqrt(mean((Pred[[1]]$state$pred$Ti - AllDat$yTi4)^2))
rmse1 <- sqrt(mean(res1^2, na.rm = TRUE))
rmse2 <- sqrt(mean(res2^2, na.rm = TRUE))
rmse3 <- sqrt(mean(res3^2, na.rm = TRUE))
rmse4 <- sqrt(mean(res4^2, na.rm = TRUE))

rmse1
rmse2
rmse3
rmse4

acf(res1, lag.max = 30)


