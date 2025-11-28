library(ctsmr)
library(splines)
source("CE3/Data/sdeTiTm.R")
load("CE3/Data/Exercise3.RData")
fit1 <- sdeTiTm(AllDat,AllDat$yTi4,AllDat$Ph2)

summary(fit1,extended=TRUE)

Hour <- as.numeric(strftime(AllDat$date, format="%H"))

Pred <- predict(fit1)
plot(Pred[[1]]$state$pred$Ti - AllDat$yTi4 ~ Hour)
rmse1 <- sqrt(mean((Pred[[1]]$state$pred$Ti - AllDat$yTi4)^2))
# What is going on 10 AM?
# Try to fir a varying effective window area


plot(AllDat$Gv ~ Hour)


idx <- (Hour>8 & Hour < 23) # It is impossible to fit a window area for the hours without any sun, so we limit the window area estimation to the hours with sun.
bs = bs(Hour[idx],df=5,intercept=TRUE) 

# What does the splines look like?
plot(bs[14:27,1],type='l')
lines(bs[ 14:27,2])
lines(bs[ 14:27,3])
lines(bs[ 14:27,4])
lines(bs[ 14:27,5])

bs1 <- bs2 <- bs3 <- bs4 <- bs5 <- bs6 <- numeric(dim(AllDat)[1])

bs1[idx] = bs[,1]
bs2[idx] = bs[,2]
bs3[idx] = bs[,3]
bs4[idx] = bs[,4]
bs5[idx] = bs[,5]

AllDat$bs1 = bs1
AllDat$bs2 = bs2
AllDat$bs3 = bs3
AllDat$bs4 = bs4
AllDat$bs5 = bs5


### You will have to implement sdeTITmAv ###
source("CE3/data/sdeTiTm_spline.r")
fit2 <- sdeTiTm_spline(AllDat,AllDat$yTi4,AllDat$Ph2)

plot(bs[14:27,1]*fit2$xm[3]+bs[14:27,2]*fit2$xm[4]+bs[14:27,3]*fit2$xm[5]+bs[14:27,4]*fit2$xm[6]+bs[14:27,5]*fit2$xm[7],type='l')

summary(fit2, extended = TRUE)

Hour <- as.numeric(strftime(AllDat$date, format="%H"))

Pred2 <- predict(fit2)
plot(Pred2[[1]]$state$pred$Ti - AllDat$yTi4 ~ Hour)
rmse2 <- sqrt(mean((Pred2[[1]]$state$pred$Ti - AllDat$yTi4)^2))

### You will have to implement sdeTITmAv ###
source("CE3/data/sdeTiTm_heatfrac.r")
fit3 <- sdeTiTm_heatfrac(AllDat,AllDat$yTi4,AllDat$Ph2)
summary(fit3, extended = TRUE)
Pred3 <- predict(fit3)
plot(Pred3[[1]]$state$pred$Ti - AllDat$yTi4 ~ Hour)
rmse3 <- sqrt(mean((Pred3[[1]]$state$pred$Ti - AllDat$yTi4)^2))

source("CE3/data/sdteTiTm_T3T4.r") # Removed a2 spline basis as well
fit4 <- sdeTiTm_T3T4(AllDat,AllDat$yTi4,AllDat$Ph2)
summary(fit4, extended = TRUE)
Pred4 <- predict(fit4)
plot(Pred4[[1]]$state$pred$Ti - AllDat$yTi4 ~ Hour)
rmse4 <- sqrt(mean((Pred4[[1]]$state$pred$Ti - AllDat$yTi4)^2))

plot(bs[14:27,1]*fit2$xm[3]+bs[14:27,3]*fit2$xm[5]+bs[14:27,4]*fit2$xm[6]+bs[14:27,5]*fit2$xm[7],type='l')

# RMSE
print('RMSE:')
print(rmse1)
print('RMSE with splines:')
print(rmse2)
print('RMSE with splines & fractional heating:')
print(rmse3)
print('RMSE with splines & T3:')
print(rmse4)

# Plotting data
plot(AllDat$Ph2 ~ Hour)
