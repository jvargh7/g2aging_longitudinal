
hrs_unique <- hrs_unique %>% mutate(
  h_heavydrinker = case_when(h_alcohol_days >= 3 ~ 1,
                             TRUE ~ 0),
  w_heavydrinker = case_when(w_alcohol_days >= 3 ~ 1,
                             TRUE ~ 0),
  hh_htn = case_when(h_htn == 1 & w_htn == 1 ~ 1,
                     TRUE ~ 0),  # Adding an outcome for TRUE condition
  hh_lengthmar = (h_lengthmar + w_lengthmar) / 2,  # Correcting the calculation
  hh_lengthmar_ge10 = case_when(hh_lengthmar >= 10 ~ 1,
                                TRUE ~ 0)
)


# create long format data, variable - gender

hrs_long <- bind_rows(hrs_unique %>%
                        dplyr::select(-starts_with("w_"),w_htn) %>%
                        dplyr::rename_with(~ gsub("^h_", "", .), starts_with("h_")) %>%
                        mutate(gender = "male"), 
                      hrs_unique %>%
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

hrs_long_svy <- hrs_long %>% 
  as_survey_design(ids = psu,
                   strata  = strata,
                   weight = normalizedweight,
                   nest = TRUE,
                   variance = "YG")

# N
hrs_unique %>%
  distinct(h_hhidpn) %>%
  n_distinct()

# htn for both 
hrs_unique %>%
  dplyr::filter(h_htn_diagnosed == 1 & w_htn_diagnosed == 1) %>%
  distinct(h_hhidpn) %>%
  n_distinct()

hrs_unique %>%
  dplyr::filter(h_htn_diagnosed == 1 & w_htn_diagnosed == 1) %>%
  nrow()

# htn = 0/NA for either
hrs_unique %>%
  dplyr::filter((h_htn_diagnosed == 0 | NA ) 
                | (w_htn_diagnosed == 0 | NA)) %>%
  distinct(h_hhidpn) %>%
  n_distinct()

hrs_unique %>%
  dplyr::filter((h_htn_diagnosed == 0 | NA) 
                | (w_htn_diagnosed == 0 | NA)) %>%
  nrow()

hrs_unique %>%
  dplyr::filter(
    ((h_htn_diagnosed == 0 | NA) & !is.na(h_sbp) & !is.na(h_dbp)) |
      ((w_htn_diagnosed == 0 | NA) & !is.na(w_sbp) & !is.na(w_dbp))
  ) %>%
  distinct(h_hhidpn) %>%
  n_distinct()

hrs_unique %>%
  dplyr::filter(
    ((h_htn_diagnosed == 0 | NA) & !is.na(h_sbp) & !is.na(h_dbp)) |
      ((w_htn_diagnosed == 0 | NA) & !is.na(w_sbp) & !is.na(w_dbp))
  ) %>%
  nrow()
