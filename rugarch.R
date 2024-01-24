library(magrittr)
library(dplyr)
library(rugarch)
set.seed(1000)

sim_series <- sim_series %>%
  mutate(past_return = lag(return))

sim_series <- cbind(sim_series, mean = 1)

sim_series <- sim_series %>%
  mutate(past_return_sq = past_return^2) %>% mutate(return_sq = return^2)

# Rugarch
models = list()

# GARCH Simulated Data
spec.1 = ugarchspec(
  variance.model = list(garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
  distribution.model = "norm")

models$GARCH_sim = ugarchfit(spec.1,
                          data = sim_series$return[1:5000],
                          fit.control = list(stationarity = 1,
                                             fixed.se = 0,
                                             scale = 0,
                                             rec.init = 'all',
                                             trunclag = 1000),
                          solver = "hybrid")

models$GARCH_real = ugarchfit(spec.1,
                              data = sp500$return,
                              fit.control = list(stationarity = 1,
                                                 fixed.se = 0,
                                                 scale = 0,
                                                 rec.init = 'all',
                                                 trunclag = 5000))

# Coefficients
models$GARCH_sim@fit$matcoef

# Relevant output
residuals <- data.frame(GARCH_sim = models$GARCH_sim@fit$residuals)
cond_volatility <- data.frame(GARCH_sim = sigma(models$GARCH_sim),
                              act_vol_sim = vol_sim)

cond_volatility <- cond_volatility %>% mutate(Index = seq_along(act_vol_sim))


