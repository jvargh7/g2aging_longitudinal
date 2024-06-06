
source("charls/g2ach01_input_data.R")
source("functions/svysummary.R")

# table 1 - earliest wave

charls_ewave <- charls_long %>% 
  group_by(id) %>% 
  dplyr::filter(wave == min(wave)) %>% 
  ungroup() 

charls_ewave_svy <- charls_ewave %>% 
  as_survey_design(.data = .,
                   #ids = PSU_weight, strata = strata_weight,
                   weight = normalizedweight,
                   nest = TRUE,
                   variance = "YG", pps = "brewer")

continuous_vars <- c("age","sbp","dbp","height","weight","hhsampleweight","bmi",
                    "moderate_pa","vigorous_pa",
                    "hh_size","hh_wealth","hh_income",
                    "hh_lengthmar")

proportion_vars <- c("diagnosed_bp","medication_bp","diagnosed_dm","medication_dm",
                     "heavydrinker","retirement","smokeever","smokecurr","alcohol",
                     "lengthmar_ge10","hh_lengthmar_ge10","htn")

grouped_vars <- c("laborforce","smoke","education_h","hh_incometertile",
                  "hh_wealthquintile")


charls_sy <- svysummary(charls_ewave_svy,
                      c_vars = continuous_vars,
                      p_vars = proportion_vars,
                      g_vars = grouped_vars,
                      id_vars = "gender"
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"));

charls_sy_total <- svysummary(charls_ewave_svy,
                            c_vars = continuous_vars,
                            p_vars = proportion_vars,
                            g_vars = grouped_vars
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"));

charls_table1 <- bind_rows(charls_sy_total %>% mutate(gender = "Total"),
          charls_sy) %>% 
  write_csv(.,path = "charls/g2alc05_descriptive characteristics.csv")


with(charls_ewave,table(gender))







