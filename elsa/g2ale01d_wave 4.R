gc();rm(list=ls());source(".Rprofile")

h_elsa_g2 <- haven::read_dta(paste0(path_g2a_data,"/elsa/h_elsa_g2.dta"),
                             col_select = c("h4coupid","idauniq","s4idauniq","ragender","s4gender","r1strat","r4strat"))


g2aelsa_r_variables <- readxl::read_excel("elsa/G2A ELSA Longitudinal Variable List.xlsx",sheet="wave4") %>% 
  rename("selected" = harmonized_r) %>% 
  dplyr::select(level,new_var,selected) %>% 
  dplyr::filter(!is.na(selected))

g2aelsa_s_variables <- readxl::read_excel("elsa/G2A ELSA Longitudinal Variable List.xlsx",sheet="wave4") %>% 
  rename("selected" = harmonized_s) %>% 
  dplyr::select(level,new_var,selected) %>% 
  dplyr::filter(!is.na(selected))

source("elsa/g2alelsa_individual data.R")

saveRDS(male,paste0(path_g2a_longitudinal_folder,"/working/elsa/G2A ELSA Wave 4 male.RDS"))  
saveRDS(female,paste0(path_g2a_longitudinal_folder,"/working/elsa/G2A ELSA Wave 4 female.RDS"))  
saveRDS(heterosexual_couples_hh,paste0(path_g2a_longitudinal_folder,"/working/elsa/G2A ELSA Wave 4 heterosexual couples.RDS"))  
saveRDS(msm_couples_hh,paste0(path_g2a_longitudinal_folder,"/working/elsa/G2A ELSA Wave 4 msm couples.RDS"))  
saveRDS(wsw_couples_hh,paste0(path_g2a_longitudinal_folder,"/working/elsa/G2A ELSA Wave 4 wsw couples.RDS"))  


