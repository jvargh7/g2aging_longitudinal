
source("elsa/g2ach01_input_data.R")
source("functions/svysummary.R")

# table 1 - earliest wave

elsa_ewave <- elsa_long %>% 
  group_by(personid) %>% 
  dplyr::filter(wave == min(wave)) %>% 
  ungroup() 

elsa_ewave_svy <- elsa_ewave %>% 
  as_survey_design(ids = psu,
                   strata  = strata,
                   weight = normalizedweight,
                   nest = TRUE,
                   variance = "YG")


continuous_vars <- c("age","sbp","dbp","height","weight","bmi","eduyr","moderate_pa",
                     "vigorous_pa","hh_size","children","lengthmar","hh_wealth",
                     "hh_income","hh_consumption","hh_lengthmar")

proportion_vars <- c("diagnosed_bp","medication_bp","diagnosed_dm","medication_dm",
                     "heavy_drinker","employment","retirement","smokeever","smokecurr","alcohol",
                     "lengthmar_ge10","hh_lengthmar_ge10","insurance","htn")

grouped_vars <- c("laborforce","smoke","education_h","hh_incometertile",
                  "hh_wealthquintile","hh_consumptionquintile","race","religion")



elsa_sy <- svysummary(elsa_ewave_svy,
                     c_vars = continuous_vars,
                     p_vars = proportion_vars,
                     g_vars = grouped_vars,
                     id_vars = "gender"
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"));

elsa_sy_total <- svysummary(elsa_ewave_svy,
                           c_vars = continuous_vars,
                           p_vars = proportion_vars,
                           g_vars = grouped_vars
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"));

elsa_table1 <- bind_rows(elsa_sy_total %>% mutate(gender = "Total"),
          elsa_sy) %>% 
  write_csv(.,path = "elsa/g2ale04_descriptive characteristics.csv")


with(elsa_ewave,table(gender))









