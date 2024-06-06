
charls_unique <- charls_unique %>% mutate(
  hh_htn = case_when(h_htn == 1 & w_htn == 1 ~ 1,
                     TRUE ~ 0))

# create long format data, variable - gender

charls_long <- bind_rows(charls_unique %>%
                           dplyr::select(-starts_with("w_"),w_htn) %>%
                           dplyr::rename_with(~ gsub("^h_", "", .), starts_with("h_")) %>%
                           mutate(gender = "male"), 
                         charls_unique %>%
                           dplyr::select(-starts_with("h_"),h_htn) %>%
                           dplyr::rename_with(~ gsub("^w_", "", .), starts_with("w_")) %>%
                           mutate(gender = "female")) %>% 
  mutate(w_htn = case_when(
    gender == "female" ~ htn,
    TRUE ~ w_htn)) %>% 
  mutate(h_htn = case_when(
    gender == "male" ~ htn,
    TRUE ~ h_htn))

# Survey design

charls_long_svy <- charls_long %>% 
  as_survey_design(.data = .,
                   #ids = PSU_weight, strata = strata_weight,
                   weight = normalizedweight,
                   nest = TRUE,
                   variance = "YG", pps = "brewer")

