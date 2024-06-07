rm(list=ls());gc();source(".Rprofile")

elsa <- add_normalizedweight(paste0(path_g2a_longitudinal_folder, "/working/elsa couples.RDS"),
                        "h_sampleweight", "w_sampleweight", "h_personid", "w_personid")

elsa <- elsa %>% mutate(
  hh_htn = case_when(h_htn == 1 & w_htn == 1 ~ 1,
                     TRUE ~ 0),
  hh_children = (h_children+w_children)/2)

# obs = 5465
elsa %>%
  distinct(h_personid) %>%
  n_distinct() # 2380
elsa %>%
  distinct(w_personid) %>%
  n_distinct() # 2380

# create long format data, variable - gender

elsa_long <- bind_rows(elsa %>%
                        dplyr::select(-starts_with("w_"),w_htn) %>%
                        dplyr::rename_with(~ gsub("^h_", "", .), starts_with("h_")) %>%
                        mutate(gender = "male"), 
                      elsa %>%
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

elsa_long_svy <- elsa_long %>% 
  as_survey_design(ids = psu,
                   strata  = strata,
                   weight = normalizedweight,
                   nest = TRUE,
                   variance = "YG")

