
source("hrs/g2ach01_input_data.R")
source("functions/svysummary.R")

# table 1 - earliest wave

hrs_ewave <- hrs_long %>% 
  group_by(hhidpn) %>% 
  dplyr::filter(wave == min(wave)) %>% 
  ungroup() 

hrs_ewave_svy <- hrs_ewave %>% 
  as_survey_design(ids = psu,
                   strata  = strata,
                   weight = normalizedweight,
                   nest = TRUE,
                   variance = "YG")


continuous_vars <- c("age","sbp","dbp","height","weight","physicalweight","bmi",
                     "waistcircumference",
                     "moderate_pa","vigorous_pa",
                     "hh_size","lengthmar",
                    "hh_children","alcohol_days")

proportion_vars <- c("diagnosed_bp","htn_diagnosed","htn_treated","diagnosed_dm",
                     "alcohol","lengthmar_ge10","htn")

grouped_vars <- c("laborforce","smoke","education_h","hh_incometertile",
                  "hh_wealthquintile","race","ethnicity","religion")



hrs_sy <- svysummary(hrs_ewave_svy,
                     c_vars = continuous_vars,
                     p_vars = proportion_vars,
                     g_vars = grouped_vars,
                     id_vars = "gender"
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"));

hrs_sy_total <- svysummary(hrs_ewave_svy,
                           c_vars = continuous_vars,
                           p_vars = proportion_vars,
                           g_vars = grouped_vars
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"));

hrs_table1 <- bind_rows(hrs_sy_total %>% mutate(gender = "Total"),
          hrs_sy) %>% 
  write_csv(.,path = "hrs/g2alh06_descriptive characteristics.csv")


with(hrs_ewave,table(gender))



# merge 3 tables

hrs_table1_wide <- hrs_table1 %>% 
  dplyr::filter(gender != "Total") %>% 
  dplyr::select(variable, group, est_ci,gender) %>% 
  pivot_wider(
    names_from = gender,
    values_from = est_ci,
    names_prefix = "HRS_"
  ) 

elsa_table1_wide <- elsa_table1 %>% 
  dplyr::filter(gender != "Total") %>% 
  dplyr::select(variable, group, est_ci,gender) %>% 
  pivot_wider(
    names_from = gender,
    values_from = est_ci,
    names_prefix = "ELSA_"
  )

charls_table1_wide <- charls_table1 %>% 
  dplyr::filter(gender != "Total") %>% 
  dplyr::select(variable, group, est_ci,gender) %>% 
  pivot_wider(
    names_from = gender,
    values_from = est_ci,
    names_prefix = "CHARLS_"
  )


table1 <- full_join(hrs_table1_wide, 
                    elsa_table1_wide,
                    by = c("variable","group")) %>% 
          full_join(charls_table1_wide,
                    by = c("variable","group")) %>% 
  write_csv(.,path = "paper/table_descriptive characteristics.csv")


hrs_ewave %>%
  dplyr::filter(gender == "female") %>% 
  distinct(hhidpn) %>%
  n_distinct() # 6464
hrs_ewave %>%
  dplyr::filter(gender == "male") %>% 
  distinct(hhidpn) %>%
  n_distinct()
