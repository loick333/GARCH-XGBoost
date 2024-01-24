library(MSGARCH)
library(zoo)
set.seed(1000)
spec <- CreateSpec(variance.spec = list(model = c("sGARCH")),
                   distribution.spec = list(distribution = c("norm")),
                   switch.spec = list(do.mix = FALSE, K = 2))

print(spec)

# Transform data to right format
zoo_series <- zoo(simpleReturns$BAC, order.by = simpleReturns$Date)

#Fit to Data

# MS(2)-GARCH(1,1)
spec <- CreateSpec(variance.spec = list(model = c("sGARCH")),
                   distribution.spec = list(distribution = c("norm")),
                   switch.spec = list(do.mix = FALSE, K = 2))



zoo_series <- zoo(sp500$return, order.by = sp500$Date)
fit <- FitML(spec = spec,
               data = zoo_series)
summary(fit)

#Simulate 10000 iterations
sim.series <- simulate(object = spec, par = fit$par, nahead= 5000L, nsim = 1L)
vol_sim <- sim.series$CondVol[,,1]
sim.series <- as.vector(sim.series$draw)

sim_series <- data.frame(time = seq_along(sim.series), return = sim.series)



