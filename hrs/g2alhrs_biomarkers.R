gc();rm(list=ls());source(".Rprofile")

library(haven)

g2ahrs_biomarker_variables_wave8 <- readxl::read_excel("hrs/G2A HRS Longitudinal Variable List.xlsx",sheet="biomarker") %>% 
  rename("selected" = wave8) %>% 
  dplyr::select(level,new_var,selected) %>% 
  dplyr::filter(!is.na(selected)) 

g2ahrs_biomarker_variables_wave9 <- readxl::read_excel("hrs/G2A HRS Longitudinal Variable List.xlsx",sheet="biomarker") %>% 
  rename("selected" = wave9) %>% 
  dplyr::select(level,new_var,selected) %>% 
  dplyr::filter(!is.na(selected)) 

g2ahrs_biomarker_variables_wave10 <- readxl::read_excel("hrs/G2A HRS Longitudinal Variable List.xlsx",sheet="biomarker") %>% 
  rename("selected" = wave10) %>% 
  dplyr::select(level,new_var,selected) %>% 
  dplyr::filter(!is.na(selected)) 

g2ahrs_biomarker_variables_wave11 <- readxl::read_excel("hrs/G2A HRS Longitudinal Variable List.xlsx",sheet="biomarker") %>% 
  rename("selected" = wave11) %>% 
  dplyr::select(level,new_var,selected) %>% 
  dplyr::filter(!is.na(selected)) 

g2ahrs_biomarker_variables_wave12 <- readxl::read_excel("hrs/G2A HRS Longitudinal Variable List.xlsx",sheet="biomarker") %>% 
  rename("selected" = wave12) %>% 
  dplyr::select(level,new_var,selected) %>% 
  dplyr::filter(!is.na(selected)) 

g2ahrs_biomarker_variables_wave13 <- readxl::read_excel("hrs/G2A HRS Longitudinal Variable List.xlsx",sheet="biomarker") %>% 
  rename("selected" = wave13) %>% 
  dplyr::select(level,new_var,selected) %>% 
  dplyr::filter(!is.na(selected)) 


wave8 <- read_sav(paste0(path_g2a_longitudinal_folder,"/working/hrs raw/biomkr06/biomk06bl_r.sav")) %>% 
  rename_with(~ g2ahrs_biomarker_variables_wave8$new_var[which(g2ahrs_biomarker_variables_wave8$selected == .x)], 
              .cols = g2ahrs_biomarker_variables_wave8$selected)

wave9 <- read_sav(paste0(path_g2a_longitudinal_folder,"/working/hrs raw/biomkr08/biomk08bl_r.sav")) %>% 
  rename_with(~ g2ahrs_biomarker_variables_wave9$new_var[which(g2ahrs_biomarker_variables_wave9$selected == .x)], 
              .cols = g2ahrs_biomarker_variables_wave9$selected)

wave10 <- read_sav(paste0(path_g2a_longitudinal_folder,"/working/hrs raw/biomkr10/biomk10bl_r.sav")) %>% 
  rename_with(~ g2ahrs_biomarker_variables_wave10$new_var[which(g2ahrs_biomarker_variables_wave10$selected == .x)], 
              .cols = g2ahrs_biomarker_variables_wave10$selected)

wave11 <- read_sav(paste0(path_g2a_longitudinal_folder,"/working/hrs raw/biomkr12/biomk12bl_r.sav")) %>% 
  rename_with(~ g2ahrs_biomarker_variables_wave11$new_var[which(g2ahrs_biomarker_variables_wave11$selected == .x)], 
              .cols = g2ahrs_biomarker_variables_wave11$selected)

wave12 <- read_sav(paste0(path_g2a_longitudinal_folder,"/working/hrs raw/BIOMK14BL/biomk14bl.sav")) %>% 
  rename_with(~ g2ahrs_biomarker_variables_wave12$new_var[which(g2ahrs_biomarker_variables_wave12$selected == .x)], 
              .cols = g2ahrs_biomarker_variables_wave12$selected)

wave13 <- read_sav(paste0(path_g2a_longitudinal_folder,"/working/hrs raw/BIOMK16BL/BIOMK16BL_R.sav")) %>% 
  rename_with(~ g2ahrs_biomarker_variables_wave13$new_var[which(g2ahrs_biomarker_variables_wave13$selected == .x)], 
              .cols = g2ahrs_biomarker_variables_wave13$selected)

biomarkers <- bind_rows(
  wave8 %>% mutate(wave = 8, year = 2006),
  wave9 %>% mutate(wave = 9, year = 2008),
  wave10 %>% mutate(wave = 10, year = 2010),
  wave11 %>% mutate(wave = 11, year = 2012),
  wave12 %>% mutate(wave = 12, year = 2014),
  wave13 %>% mutate(wave = 13, year = 2016)
) %>% 
  arrange(hhid,pn,wave) %>% 
  dplyr::select(hhid,pn,wave,year,biowgtr,blversion,everything()) %>% 
  mutate(pn = as.numeric(pn),
         hhid = paste0(hhid,"0"))

saveRDS(biomarkers,paste0(path_g2a_longitudinal_folder,"/working/hrs biomarkers.RDS"))
