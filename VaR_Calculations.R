library(ggplot2)
set.seed(1000)
# VaR Calculations for GARCH on Simulated Data
# REEVALUATE EVERY 1500 thats important
mod_sim = ugarchroll(spec.1, data = sim_series$return, n.ahead = 1,
                      n.start = 1000, refit.every = 1, refit.window = "moving",
                      solver = "hybrid", fit.control = list(),
                      calculate.VaR = TRUE, VaR.alpha = c(0.05, 0.025, 0.05),
                      keep.coef = TRUE)

# Dataframe of VaR levels and Realizations
roll_VaR_sim = as.data.frame(mod_sim, which = 'VaR')
colnames(roll_VaR_sim) <- c('alpha_1', 'alpha_3', 'alpha_5', 'realized')
plot(roll_VaR_sim$alpha_1)
roll_VaR_sim <- roll_VaR_sim %>% mutate(Index = seq_along(realized))

gfg_plot_sim <- ggplot(roll_VaR_sim[1:600,], aes(x = Index)) + 
  geom_line(aes(y = realized), color = "black") + 
  geom_line(aes(y = alpha_5), color = "green")

#VaR from MSGarch
p <- 0.05
NormalVaR=function(p,sd=1) return(-qnorm(p,sd=sd))

#VaR = round( NormalVaR(p=p,sd=xgb_pred_sigma),5)

vol_sim_data_frame <- data.frame(vol_sim)
ms_garch_actual_VaR <- apply(vol_sim_data_frame, 1, NormalVaR, p = 0.05)

