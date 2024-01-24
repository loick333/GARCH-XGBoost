library(dplyr)
library(tidyr)

violation_analysis <- read.csv('violation_analysis.csv')

violation_analysis <- data.frame(xgb_VaR_five = VaR*(-1),
                        garch_VaR_five = roll_VaR_sim$alpha_5[-1],
                        actual_returns = sim_series$return[1002:5000])

violation_analysis <- violation_analysis %>%
  mutate(garch_violations = actual_returns < garch_VaR_five) %>%
  mutate(xgb_violations = actual_returns < xgb_VaR_five) %>%
  mutate(time = seq_along(xgb_VaR_five))

violation_analysis <- violation_analysis %>% mutate(not_equal = xgb_violations != garch_violations)
sum(violation_analysis$not_equal)
true_false <- ggplot(subset(violation_analysis, not_equal == TRUE), aes(x = time)) +
  geom_point(aes(y = not_equal), color = 'black') + theme_classic() +
  labs(x = 'Time', y ='Violations')

write.csv(violation_analysis, "C:/Users/loick/OneDrive/LSE/FM_442_Risk/FM442/Risk_Measure_Boosting/violation_analysis.csv", row.names=FALSE)


figure_5 <- grid.arrange(garch_Violations, true_false, nrow=2)

GARCH_Violation_ratio <-
  sum(violation_analysis$garch_violations)/(nrow(violation_analysis)*0.05)

xgb_Violation_ratio <-
  sum(violation_analysis$xgb_violations)/(nrow(violation_analysis)*0.05)


# Indep. Test
ind_test = function(V){
  count_t_t <- sum(V & lead(V))
  count_f_f <- sum(!V & !lead(V), na.rm=TRUE)
  count_f_t <- sum(!V & lead(V), na.rm=TRUE)
  count_t_f <- sum(V & !lead(V))
  
  p_f_f <- count_f_f/(count_f_f + count_f_t)
  p_f_t <- count_f_t/(count_f_f + count_f_t)
  p_t_f <- count_t_f/(count_t_f + count_t_t)
  p_t_t <- count_t_t/(count_t_f + count_t_t)
  
  hat_p <- (count_f_t + count_t_t)/(count_t_f + count_f_t + count_f_f + count_t_t)
  
  a <- log((1-hat_p)^(count_f_f + count_t_f)) + log((hat_p)^(count_f_t + count_t_t))
  b <- log((p_f_f)^(count_f_f)) + log((p_f_t)^(count_f_t)) +
  log((p_t_f)^(count_t_f)) + log((p_t_t)^(count_t_t))
  return(-2*(a-b))
}

indep_garch = ind_test(violation_analysis$garch_violations)
indep_xgb = ind_test(violation_analysis$xgb_violations)

v=violation_analysis$garch_violations
bern_test <- function(p,v){

  lv <- length(v)
  sv <- sum(v)
  al <- log(p) * sv + log(1-p)*(lv-sv)
  bl <- log(sv/lv)*sv + log(1-sv/lv)*(lv-sv)
  return(-2*(al-bl))
}


bern_garch <- bern_test(0.05, violation_analysis$garch_violations)
bern_xgb <- bern_test(0.05, violation_analysis$xgb_violations)
