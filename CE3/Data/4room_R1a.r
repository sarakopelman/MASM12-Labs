library(ctsmr)
library(splines)
source("CE3/Data/sde4room_R1a.r") # change to correct model
load("CE3/Data/Exercise3.RData")
Hour <- as.numeric(strftime(AllDat$date, format="%H"))

AllDatJanMar <- AllDat[as.integer(format(AllDat$date, "%m")) %in% 1:3, ] # Testing only Januari and March

idx <- (Hour>8 & Hour < 23) # It is impossible to fit a window area for the hours without any sun, so we limit the window area estimation to the hours with sun.

bs = bs(Hour[idx],df=4,intercept=TRUE) 

#bs1 <- bs2 <- bs3 <- bs4 <- bs5 <- bs6 <- numeric(dim(AllDat)[1])
bs1 <- bs2 <- bs3 <- bs4 <-  numeric(dim(AllDat)[1])

bs1[idx] = bs[,1]
bs2[idx] = bs[,2]
bs3[idx] = bs[,3]
bs4[idx] = bs[,4]
#bs5[idx] = bs[,5]

AllDat$bs1 = bs1
AllDat$bs2 = bs2
AllDat$bs3 = bs3
AllDat$bs4 = bs4
#AllDat$bs5 = bs5

fit1 <- sde4room_R1a(AllDatJanMar) # change to correct model

summary(fit1)
summary(fit1, extended=TRUE)

Pred1 <- predict(fit1)
res1 <- Pred1[[1]]$state$pred$Ti1 - AllDatJanMar$yTi1
res2 <- Pred1[[1]]$state$pred$Ti2 - AllDatJanMar$yTi2
res3 <- Pred1[[1]]$state$pred$Ti3 - AllDatJanMar$yTi3
res4 <- Pred1[[1]]$state$pred$Ti4 - AllDatJanMar$yTi4
AllDatJanMar$res1 <- res1
AllDatJanMar$res2 <- res2
AllDatJanMar$res3 <- res3
AllDatJanMar$res4 <- res4


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

#legend("topright",
#       legend = c("Ti1", "Ti2"),
#       col    = c("blue", "red"),
#       lty    = 1,
#       bty    = "n")

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


