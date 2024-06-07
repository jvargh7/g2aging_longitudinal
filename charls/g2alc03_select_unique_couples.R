rm(list=ls());gc();source(".Rprofile")


charls <- add_normalizedweight(paste0(path_g2a_longitudinal_folder, "/working/charls couples.RDS"),
                               "h_indsampleweight", "w_indsampleweight", "h_id", "w_id")
# obs = 15590
charls %>%
  distinct(h_id) %>%
  n_distinct() # 8381
charls %>%
  distinct(w_id) %>%
  n_distinct() # 8387


## filter by husband
unique_h <- charls %>% 
  group_by(h_id) %>%
  dplyr::filter(n_distinct(h_spouseid) == 1) %>% 
  ungroup()
# obs = 15564
unique_h %>%
  distinct(h_id) %>%
  n_distinct() # 8370
unique_h %>%
  distinct(w_id) %>%
  n_distinct() # 8365

duplicate_h <- charls %>% 
  group_by(h_id) %>%
  dplyr::filter(n_distinct(h_spouseid) > 1) %>%
  arrange(h_id, wave) %>% 
  ungroup()
# obs = 26
duplicate_h %>%
  distinct(h_id) %>%
  n_distinct() # 11
duplicate_h %>%
  distinct(w_id) %>%
  n_distinct() # 22

earliest_spouse_h <- duplicate_h %>%
  group_by(h_id) %>%
  arrange(wave) %>%
  slice(1) %>%
  ungroup() %>% 
  dplyr::select(h_id,h_spouseid)

filtered_duplicate_h <- duplicate_h %>%
  inner_join(earliest_spouse_h, 
             by = c("h_id", "h_spouseid"))
# obs = 12
filtered_duplicate_h %>%
  distinct(h_id) %>%
  n_distinct() # 11
filtered_duplicate_h %>%
  distinct(w_id) %>%
  n_distinct() # 11

unique_h_charls <- bind_rows(unique_h,filtered_duplicate_h)
# obs = 15576
unique_h_charls %>%
  distinct(h_id) %>%
  n_distinct() # 8381
unique_h_charls %>%
  distinct(w_id) %>%
  n_distinct() # 8376

## filter by wife
unique_w <- unique_h_charls %>% 
  group_by(w_id) %>%
  dplyr::filter(n_distinct(w_spouseid) == 1) %>% 
  ungroup()
# obs = 15565
unique_w %>%
  distinct(h_id) %>%
  n_distinct() # 8371
unique_w %>%
  distinct(w_id) %>%
  n_distinct() # 8371

duplicate_w <- unique_h_charls %>% 
  group_by(w_id) %>%
  dplyr::filter(n_distinct(w_spouseid) > 1) %>%
  arrange(w_id, wave) %>% 
  ungroup()
# obs = 11
duplicate_w %>%
  distinct(h_id) %>%
  n_distinct() # 10
duplicate_w %>%
  distinct(w_id) %>%
  n_distinct() # 5

earliest_spouse_w <- duplicate_w %>%
  group_by(w_id) %>%
  arrange(wave) %>%
  slice(1) %>%
  ungroup() %>% 
  dplyr::select(w_id,w_spouseid)

filtered_duplicate_w <- duplicate_w %>%
  inner_join(earliest_spouse_w, 
             by = c("w_id", "w_spouseid"))
# obs = 5
filtered_duplicate_w %>%
  distinct(h_id) %>%
  n_distinct() # 5
filtered_duplicate_w %>%
  distinct(w_id) %>%
  n_distinct() # 5

charls_unique <- bind_rows(unique_w, filtered_duplicate_w)
# obs = 15570
charls_unique %>%
  distinct(h_id) %>%
  n_distinct() # 8376
charls_unique %>%
  distinct(w_id) %>%
  n_distinct() # 8376

