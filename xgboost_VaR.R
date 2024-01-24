library(xgboost)
library(caret)
library(dplyr)
set.seed(1000)
# VaR Calculations for XGBoost-GARCH Model

NormalVaR=function(p,sd=1) return(-qnorm(p,sd=sd))
NormalES=function(p,sd=1) return(sd*dnorm(qnorm(p))/(p))


p <- 0.05
value = 1
WE = 1000
T=length(sim_series$return)

WT=T-WE
y <- sim_series

Risk.xgboost=function(y, p, value, WE){
  #First, evaluate GARCH
  spec = ugarchspec(
    variance.model = list(garchOrder = c(1,1)),
    mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
    distribution.model = "norm")
  
  res = ugarchfit(spec,
                    data = y$return,
                    fit.control = list(stationarity = 1,
                                       fixed.se = 0,
                                       scale = 0,
                                       rec.init = 'all',
                                       trunclag = 1000),
                    solver = "hybrid")
  
  xgboost_dataset <- data.frame(sigma = res@fit$sigma,
                                residuals_sq = res@fit$residuals^2,
                                return = y$return)
  
  xgboost_dataset <- xgboost_dataset[-1,] %>%
    mutate(sigma_lag = lag(sigma)) %>%
    mutate(residuals_sq_lag = lag(residuals_sq))
  

  #Don't train on last iteration as it will be used for prediction
  x <- data.matrix(xgboost_dataset[1:(nrow(xgboost_dataset)), ] %>%
                     dplyr::select(sigma_lag, residuals_sq_lag))
  y <- xgboost_dataset$sigma
  
  xgb <- xgb.DMatrix(data = x, label = y)
  
  #watchlist <- list(train=xgb_train, test=xgb_test)
  
  xgb_model <- xgb.train(data = xgb,
                         max.depth = 4,
                         nrounds=50) # KILL AFTER 50 rounds
  
  #Take Last Value to Forecast
  xgb_pred_sigma <- xgb_model %>% stats::predict(tail(x, 1))
  
  VaR_five = round( NormalVaR(p=p,sd=xgb_pred_sigma)* value,5)
  
  return(list(risk=list(VaR=VaR_five, ES=ES),
              method="xgboost_GARCH",
              par=coef(res),
              p=p,
              value=value,
              WE=WE,
              sigma=xgb_pred_sigma))
}


#VaR=rep(NA,length=T)
VaR <- c()
for(i in (WE+1):T){
  t1=i-WE
  t2=i-1
  data=sim_series[t1:t2,]
  VaR[i-WE-1]=Risk.xgboost(y=data,p=p,value=value,WE=WE)$risk$VaR
  print(i)
  
}

write.csv(multi_VaR, "C:/Users/loick/OneDrive/LSE/FM_442_Risk/FM442/Risk_Measure_Boosting/multi_VaR.csv", row.names=FALSE)
