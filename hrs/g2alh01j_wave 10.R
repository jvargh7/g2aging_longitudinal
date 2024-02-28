gc();rm(list=ls());source(".Rprofile")
source("hrs/g2alhrs_preprocessing.R")

g2ahrs_harmonized_r_variables <- readxl::read_excel("hrs/G2A HRS Longitudinal Variable List.xlsx",sheet="wave10") %>% 
  rename("selected" = harmonized_r) %>% 
  dplyr::select(level,new_var,selected) %>% 
  dplyr::filter(!is.na(selected)) %>% 
  mutate(selected = str_to_lower(selected))

g2ahrs_harmonized_s_variables <- readxl::read_excel("hrs/G2A HRS Longitudinal Variable List.xlsx",sheet="wave10") %>% 
  rename("selected" = harmonized_s) %>% 
  dplyr::select(level,new_var,selected) %>% 
  dplyr::filter(!is.na(selected)) %>% 
  mutate(selected = str_to_lower(selected))

g2ahrs_rand_r_variables <- readxl::read_excel("hrs/G2A HRS Longitudinal Variable List.xlsx",sheet="wave10") %>% 
  rename("selected" = rand_r) %>% 
  dplyr::select(level,new_var,selected) %>% 
  dplyr::filter(!is.na(selected)) %>% 
  mutate(selected = str_to_lower(selected))

g2ahrs_rand_s_variables <- readxl::read_excel("hrs/G2A HRS Longitudinal Variable List.xlsx",sheet="wave10") %>% 
  rename("selected" = rand_s) %>% 
  dplyr::select(level,new_var,selected) %>% 
  dplyr::filter(!is.na(selected)) %>% 
  mutate(selected = str_to_lower(selected))


source("hrs/g2alhrs_individual data.R")


saveRDS(male,paste0(path_g2a_longitudinal_folder,"/working/hrs/G2A HRS Wave 10 male.RDS"))  
saveRDS(female,paste0(path_g2a_longitudinal_folder,"/working/hrs/G2A HRS Wave 10 female.RDS"))  
saveRDS(heterosexual_couples_hh,paste0(path_g2a_longitudinal_folder,"/working/hrs/G2A HRS Wave 10 heterosexual couples.RDS"))  
saveRDS(msm_couples_hh,paste0(path_g2a_longitudinal_folder,"/working/hrs/G2A HRS Wave 10 msm couples.RDS"))  
saveRDS(wsw_couples_hh,paste0(path_g2a_longitudinal_folder,"/working/hrs/G2A HRS Wave 10 wsw couples.RDS"))  

