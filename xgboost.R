library(xgboost)
library(caret)
library(dplyr)
set.seed(1000)

xgboost_dataset <- data.frame(sigma = models$GARCH_sim@fit$sigma,
                     residuals_sq = models$GARCH_sim@fit$residuals^2,
                     return = sim_series$return)

xgboost_dataset <- xgboost_dataset[-1,] %>%
  mutate(sigma_lag = lag(sigma)) %>%
  mutate(residuals_sq_lag = lag(residuals_sq))

# Test 1
train <- xgboost_dataset[1:2500,]
test <- xgboost_dataset[2501:nrow(xgboost_dataset),]

x_train <- data.matrix(train %>% dplyr::select(sigma_lag, residuals_sq_lag))
x_test <- data.matrix(test %>% dplyr::select(sigma_lag, residuals_sq_lag))

y_train <- xgboost_dataset$sigma[1:2500]
y_test <- xgboost_dataset$sigma[2501:nrow(xgboost_dataset)]

xgb_train <- xgb.DMatrix(data = x_train, label = y_train)
xgb_test <- xgb.DMatrix(data = x_test, label = y_test)

watchlist <- list(train=xgb_train, test=xgb_test)

xgb_model <- xgb.train(data = xgb_train,
                   max.depth = 4,
                   watchlist=watchlist,
                   nrounds=50)


#Evaluate with Training Data
xgb_pred_train <- xgb_model %>% stats::predict(x_train)
MSE_train = mean((y_train - xgb_pred)^2)

#Evaluate with Test Data (on which xgboost was not trained)
xgb_pred_test <- xgb_model %>% stats::predict(x_test)
MSE_test = mean((y_test - xgb_pred_test)^2)
print(c(MSE_train,MSE_test))

plot(xgb_pred_test, type = 'l')
lines(seq(1, length(y_test)), y_test, type = 'l')

plot(xgb_pred_train, type = 'l')
lines(seq(1, length(y_train)), y_train, type = 'l')

plot(xgb_pred_train, type = 'l')
lines(seq(1, 2500), vol_sim[1:2500], type = 'l')

plot((xgb_pred_train - vol_sim[1:2500])/(cond_volatility$GARCH_sim[1:2500] - vol_sim[1:2500])-1
     , type = 'l')

#Mean Bias is smaller, however, the spikes are pretty bad.
mean((xgb_pred_train - vol_sim[1:2500])/(cond_volatility$GARCH_sim[1:2500] - vol_sim[1:2500]))

training <- data.frame(xgb_pred_train)
testing <- data.frame(xgb_pred_test)
colnames(training) <- colnames(testing)
xgboost_pred <- rbind(training, testing)
 