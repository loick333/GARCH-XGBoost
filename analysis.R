library(ggplot2)
library(gridExtra)
#Create the DataFrame we will use for Analysis
simulation_df <- read.csv('simulation_df.csv')

simulation_df <- data.frame(actual_vol = vol_sim,
                            actual_VaR = ms_garch_actual_VaR,
                            actual_returns = sim_series$return)

write.csv(simulation_df, "C:/Users/loick/OneDrive/LSE/FM_442_Risk/FM442/Risk_Measure_Boosting/simulation_df.csv", row.names=FALSE)

# Simulated Returns Plot Figure 1
Simulate_returns_plot <- ggplot(sim_series, aes(x=seq_along(time), y = return)) +
  geom_line() +
  labs(x = "Time",
       y = "Returns") + theme_classic()


# Table 1 and equation 7:
summary(fit)

# Table 2:
models$GARCH_sim@fit$matcoef



#Create the DataFrame we will use for Analysis

multi_VaR <- read.csv('multi_VaR.csv')

multi_VaR <- data.frame(xgb_VaR_five = VaR*(-1),
                        garch_VaR_five = roll_VaR_sim$alpha_5[-1],
                        vol_garch = cond_volatility$GARCH_sim[1002:5000],
                        vol_xgboost = cond_volatility$GARCH_sim[1001:4999],
                        actual_vol = vol_sim[1002:5000],
                        actual_VaR = ms_garch_actual_VaR[1002:5000]*(-1),
                        actual_returns = sim_series$return[1002:5000])
write.csv(multi_VaR, "C:/Users/loick/OneDrive/LSE/FM_442_Risk/FM442/Risk_Measure_Boosting/multi_VaR.csv", row.names=FALSE)

# Volatility Plots
garch_estimatevsactual<- ggplot(multi_VaR, aes(x = seq_along(vol_garch))) + 
  geom_line(aes(y = vol_garch - actual_vol)) +
  labs(x = "Time",
       y = "Delta Volatility") + theme_classic()

vol_plot_xgb <- ggplot(multi_VaR, aes(x = seq_along(vol_garch))) + 
  geom_line(aes(y = vol_xgboost, colour = "XgBoost")) +
  geom_line(aes(y = actual_vol, colour = "Actual")) +
  labs(x = "Time",
       y = "Volatility") +
  theme_classic() +
  scale_colour_manual("", 
                      values = c("XgBoost"="black", "Actual"="red")) +
  xlab(" ")

xgb_estimatevsactual<- ggplot(multi_VaR, aes(x = seq_along(vol_garch))) + 
  geom_line(aes(y = vol_xgboost - actual_vol)) +
  labs(x = "Time",
       y = "Delta Volatility") + theme_classic()

vol_plot_garch <- ggplot(multi_VaR, aes(x = seq_along(vol_garch))) + 
  geom_line(aes(y = vol_garch, colour = "GARCH")) +
  geom_line(aes(y = actual_vol, colour = "Actual")) +
  labs(x = "Time",
       y = "Volatility") +
  theme_classic() +
  scale_colour_manual("", 
                      values = c("GARCH"="black", "Actual"="red")) +
  xlab(" ")

figure_2 <- grid.arrange(vol_plot_garch, xgb_estimatevsactual, nrow=2)
figure_3 <-grid.arrange(vol_plot_garch, garch_estimatevsactual, nrow=2)

diff_diff <- data.frame((xgb_pred_train - vol_sim[1:2500])/(cond_volatility$GARCH_sim[1:2500] - vol_sim[1:2500])-1)
colnames(diff_diff) <- c('test')

figure_4 <- ggplot(diff_diff, aes(x = seq_along(test))) + 
  geom_line(aes(y = test)) +
  labs(x = "Time",
       y = "Quotients of Delta Volatility") + theme_classic()

#VaR Plots
VaR_plot_xgb <- ggplot(multi_VaR, aes(x = seq_along(vol_garch))) + 
  geom_line(aes(y = xgb_VaR_five), color = "black") +
  geom_line(aes(y = actual_VaR), color = 'red') +
  labs(title = "VaR: XGB vs Actual",
       x = "Time",
       y = "VaR") + theme_classic()

VaR_plot_garch <- ggplot(multi_VaR, aes(x = seq_along(vol_garch))) + 
  geom_line(aes(y = garch_VaR_five), color = "black") +
  geom_line(aes(y = actual_VaR), color = 'red') +
  labs(title = "Volatility: GARCH vs Actual",
       x = "Time",
       y = "VaR") + theme_classic()

xgb_Violations <- ggplot(multi_VaR, aes(x = seq_along(vol_garch))) + 
  geom_line(aes(y = actual_returns), color = "black") + 
  geom_line(aes(y = xgb_VaR_five), color = "green") +
  labs(x = "Time",
       y = "Returns") + theme_classic() + xlim(900,1300)

garch_Violations <- ggplot(multi_VaR, aes(x = seq_along(vol_garch))) + 
  geom_line(aes(y = actual_returns), color = "black") + 
  geom_line(aes(y = garch_VaR_five), color = "green") +
  labs(x = "Time",
       y = "Returns") + theme_classic()

figure_4 <- grid.arrange(garch_Violations, xgb_Violations, nrow=2)
# Importance of features
xgb.importance(model = xgb_model)

# Independence of Violations (Null is two violations do not follow each other)
indep_garch = ind_test(violation_analysis$garch_violations)
indep_xgb = ind_test(violation_analysis$xgb_violations)

# Number of Violations (Null is matches the right number of violations)
bern_garch <- bern_test(0.05, violation_analysis$garch_violations)
bern_xgb <- bern_test(0.05, violation_analysis$xgb_violations)
