sde4room_mimj <- function(data){
  # Generate a new object of class ctsm
  model = ctsm()
  model$options$eta <- 1e-5
  
  # Add a system equation and thereby also a state
  # Gv in Ti: Aw/Ci*Gv or Tm: Aw/Cm*Gv
  # 1/R13*(Ti1-Ti3)
  model$addSystem(
    dTi1 ~ 1/Ci1*( 1/R1a*(Ta-Ti1) + 1/Rim*(Tm1-Ti1) + 1/R12*(Ti1-Ti2)  +1/R13*(Ti1-Ti3) + Ph1)*dt 
    + exp(logsigma_i)*dw1
  )
  model$addSystem(
    dTm1 ~ 1/Cm1*1/Rim*(Tm1-Ti1)*dt + exp(logsigma_m)*dw2
  )
  
  model$addSystem(
    dTi2 ~ 1/Ci2*( 1/Ria*(Ta-Ti2) + 1/Rim*(Tm2-Ti2) + 1/R12*(Ti1-Ti2) +1/R13*(Ti1-Ti3)+ 1/R23*(Ti2-Ti3) + Ph1)*dt 
    + exp(logsigma_i)*dw3
  )
  model$addSystem(
    dTm2 ~ 1/Cm2*1/Rim*(Tm2-Ti2)*dt + exp(logsigma_m)*dw4
  )
  
  model$addSystem(
    dTi3 ~ 1/Ci3*( 1/Ria*(Ta-Ti3) + 1/Rim*(Tm3-Ti3) + 1/R23*(Ti2-Ti3) + 1/R34*(Ti3-Ti4) + Ph2 + Aw3*Gv)*dt 
    + exp(logsigma_i)*dw5
  )
  model$addSystem(
    dTm3 ~ 1/Cm3*1/Rim*(Tm3-Ti3)*dt + exp(logsigma_m)*dw6
  )
  
  model$addSystem(
    dTi4 ~ 1/Ci4*( 1/Ria*(Ta-Ti4) + 1/Rim*(Tm4-Ti4) + 1/R34*(Ti3-Ti4) + Ph2 + Aw4*Gv)*dt 
    + exp(logsigma_i)*dw7
  )
  model$addSystem(
    dTm4 ~ 1/Cm4*1/Rim*(Tm4-Ti4)*dt + exp(logsigma_m)*dw8
  )
  
  # Set the names of the inputs
  model$addInput(Ta,Gv,Ph1,Ph2)
  # Set the observation equation: Ti is the state, yTi is the measured output
  model$addObs(yTi1 ~ Ti1)
  model$addObs(yTi2 ~ Ti2)
  model$addObs(yTi3 ~ Ti3)
  model$addObs(yTi4 ~ Ti4)
  
  # Set the variance of the measurement error
  model$setVariance(yTi1 ~ exp(meas_var))
  model$setVariance(yTi2 ~ exp(meas_var))
  model$setVariance(yTi3 ~ exp(meas_var))
  model$setVariance(yTi4 ~ exp(meas_var))
  
  ##----------------------------------------------------------------
  # Set the initial value (for the optimization) of the value of the state at the starting time point
  model$setParameter(Ti1 = c(init = 23, lb = 0, ub = 40))
  model$setParameter(Tm1 = c(init = 23, lb = 0, ub = 40))
  model$setParameter(Ti2 = c(init = 23, lb = 0, ub = 40))
  model$setParameter(Tm2 = c(init = 22, lb = 0, ub = 40))
  model$setParameter(Ti3 = c(init = 22, lb = 0, ub = 40))
  model$setParameter(Tm3 = c(init = 24, lb = 0, ub = 40))
  model$setParameter(Ti4 = c(init = 24, lb = 0, ub = 40))
  model$setParameter(Tm4 = c(init = 22, lb = 0, ub = 40))
  ##----------------------------------------------------------------
  # Set the initial value for the optimization
  model$setParameter(Ci1 = c(init = 8, lb = 1E-5, ub = 1E5))
  model$setParameter(Ci2 = c(init = 24, lb = 1E-5, ub = 1E5))
  model$setParameter(Ci3 = c(init = 31, lb = 1E-5, ub = 1E5))
  model$setParameter(Ci4 = c(init = 20, lb = 1E-5, ub = 1E5))
  
  model$setParameter(Cm1 = c(init = 962, lb = 1E-5, ub = 1E5))
  model$setParameter(Cm2 = c(init = 14000, lb = 1E-5, ub = 1E5))
  model$setParameter(Cm3 = c(init = 13000, lb = 1E-5, ub = 1E5))
  model$setParameter(Cm4 = c(init = 18000, lb = 1E-5, ub = 1E5))
  
  model$setParameter(Ria = c(init = 3, lb = 1E-4, ub = 1E5))
  model$setParameter(Rim = c(init = 0.2, lb = 1E-4, ub = 1E5))  
  model$setParameter(R1a = c(init = 11, lb = 1E-4, ub = 1E5))
  
  model$setParameter(R12 = c(init = 0.4, lb = 1E-4, ub = 1E5))
  model$setParameter(R13 = c(init = 50, lb = 1E-4, ub = 1E5))
  model$setParameter(R23 = c(init = 30, lb = 1E-4, ub = 1E5))
  model$setParameter(R34 = c(init = 20, lb = 1E-4, ub = 1E5))
  
  model$setParameter(Aw3 = c(init = 8, lb = 1E-2, ub = 7.5+4.8+5))
  model$setParameter(Aw4 = c(init = 8, lb = 1E-2, ub = 7.5+4.8+5))
  
  model$setParameter(logsigma_i = c(init = -2, lb = -30, ub = 10))
  model$setParameter(logsigma_m = c(init = -1, lb = -30, ub = 10))
  model$setParameter(meas_var = c(init = -12, lb = -50, ub = 10))
  
  ##----------------------------------------------------------------    
  
  # Run the parameter optimization
  
  fit = model$estimate(data,firstorder = TRUE)
  return(fit)
}