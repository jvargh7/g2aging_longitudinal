rm(list=ls());gc();source(".Rprofile")


hrs <- add_normalizedweight(paste0(path_g2a_longitudinal_folder, "/working/hrs couples.RDS"),
                            "h_indsampleweight", "w_indsampleweight", "h_hhidpn", "w_hhidpn")
# obs = 12480
hrs %>%
  distinct(h_hhidpn) %>%
  n_distinct() # 6488
hrs %>%
  distinct(w_hhidpn) %>%
  n_distinct() # 6503

## filter by husband
unique_h <- hrs %>% 
  group_by(h_hhidpn) %>%
  dplyr::filter(n_distinct(h_spouseidpn) == 1) %>% 
  ungroup()
#obs = 12388
unique_h %>%
  distinct(h_hhidpn) %>%
  n_distinct() # 6449
unique_h %>%
  distinct(w_hhidpn) %>%
  n_distinct() # 6427

duplicate_h <- hrs %>% 
  group_by(h_hhidpn) %>%
  dplyr::filter(n_distinct(h_spouseidpn) > 1) %>%
  arrange(h_hhidpn, wave) %>% 
  ungroup()
# obs = 92
duplicate_h %>%
  distinct(h_hhidpn) %>%
  n_distinct() # 39
duplicate_h %>%
  distinct(w_hhidpn) %>%
  n_distinct() # 78

earliest_spouse_h <- duplicate_h %>%
  group_by(h_hhidpn) %>%
  arrange(wave) %>%
  slice(1) %>%
  ungroup() %>% 
  dplyr::select(h_hhidpn,h_spouseidpn)
# obs = 39
filtered_duplicate_h <- duplicate_h %>%
  inner_join(earliest_spouse_h, 
             by = c("h_hhidpn", "h_spouseidpn"))
# obs = 46
filtered_duplicate_h %>%
  distinct(h_hhidpn) %>%
  n_distinct() # 39
filtered_duplicate_h %>%
  distinct(w_hhidpn) %>%
  n_distinct() # 39
filtered_duplicate_h %>%
  distinct(w_hhidpn,h_hhidpn) %>%
  n_distinct()

unique_h_hrs <- bind_rows(unique_h,filtered_duplicate_h)
# obs = 12434
unique_h_hrs %>%
  distinct(h_hhidpn) %>%
  n_distinct() # 6488
unique_h_hrs %>%
  distinct(w_hhidpn) %>%
  n_distinct() # 6464


## filter by wife
unique_w <- unique_h_hrs %>% 
  group_by(w_hhidpn) %>%
  dplyr::filter(n_distinct(w_spouseidpn) == 1) %>% 
  ungroup()
# obs = 12377
unique_w %>%
  distinct(h_hhidpn) %>%
  n_distinct() # 6440
unique_w %>%
  distinct(w_hhidpn) %>%
  n_distinct() # 6440

duplicate_w <- unique_h_hrs %>% 
  group_by(w_hhidpn) %>%
  dplyr::filter(n_distinct(w_spouseidpn) > 1) %>%
  arrange(w_hhidpn, wave) %>% 
  ungroup()
# obs = 57
duplicate_w %>%
  distinct(h_hhidpn) %>%
  n_distinct() # 48
duplicate_w %>%
  distinct(w_hhidpn) %>%
  n_distinct() # 24

earliest_spouse_w <- duplicate_w %>%
  group_by(w_hhidpn) %>%
  arrange(wave) %>%
  slice(1) %>%
  ungroup() %>% 
  dplyr::select(w_hhidpn,w_spouseidpn)
# obs = 24
filtered_duplicate_w <- duplicate_w %>%
  inner_join(earliest_spouse_w, 
             by = c("w_hhidpn", "w_spouseidpn"))
# obs = 27
filtered_duplicate_w %>%
  distinct(h_hhidpn) %>%
  n_distinct() # 24
filtered_duplicate_w %>%
  distinct(w_hhidpn) %>%
  n_distinct() # 24

hrs_unique <- bind_rows(unique_w, filtered_duplicate_w)
# obs = 12404
hrs_unique %>%
  distinct(h_hhidpn) %>%
  n_distinct() # 6464
hrs_unique %>%
  distinct(w_hhidpn) %>%
  n_distinct() # 6464


hrs_ewave <- hrs_unique %>% 
  group_by(h_hhidpn,w_hhidpn) %>% 
  dplyr::filter(wave == min(wave)) %>% 
  ungroup() 

