
couples <- readRDS(paste0(path_g2a_longitudinal_folder,"/working/hrs couples.RDS"))

analytic_sample <- couples %>% 
  dplyr::filter(!is.na(w_lagged_sbp),!is.na(h_lagged_sbp))

library(lme4)

w0 <- lmer("w_sbp ~ w_lagged_sbp + h_lagged_sbp + wave + (1|w_hhidpn)",data=analytic_sample)

w0 %>% 
  broom.mixed::tidy(.)

library(geepack)

w0 <- lmer("w_bmi ~ w_lagged_bmi + h_lagged_bmi + wave + (1|w_hhidpn)",data=analytic_sample)
summary(w0)
