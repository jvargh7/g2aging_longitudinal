gc();rm(list=ls());source(".Rprofile")


g2acharls_r_variables <- readxl::read_excel("charls/G2A CHARLS Longitudinal Variable List.xlsx",sheet="wave2") %>% 
  rename("selected" = harmonized_r) %>% 
  dplyr::select(level,new_var,selected) %>% 
  dplyr::filter(!is.na(selected))

g2acharls_s_variables <- readxl::read_excel("charls/G2A CHARLS Longitudinal Variable List.xlsx",sheet="wave2") %>% 
  rename("selected" = harmonized_s) %>% 
  dplyr::select(level,new_var,selected) %>% 
  dplyr::filter(!is.na(selected))

source("charls/g2alcharls_individual data.R")

saveRDS(male,paste0(path_g2a_longitudinal_folder,"/working/charls/G2A CHARLS Wave 2 male.RDS"))  
saveRDS(female,paste0(path_g2a_longitudinal_folder,"/working/charls/G2A CHARLS Wave 2 female.RDS"))  
saveRDS(heterosexual_couples_hh,paste0(path_g2a_longitudinal_folder,"/working/charls/G2A CHARLS Wave 2 heterosexual couples.RDS"))  
saveRDS(msm_couples_hh,paste0(path_g2a_longitudinal_folder,"/working/charls/G2A CHARLS Wave 2 msm couples.RDS"))  
saveRDS(wsw_couples_hh,paste0(path_g2a_longitudinal_folder,"/working/charls/G2A CHARLS Wave 2 wsw couples.RDS"))  

