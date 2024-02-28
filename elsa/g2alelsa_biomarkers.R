gc();rm(list=ls());source(".Rprofile")

library(haven)

g2aelsa_biomarker_variables_wave2 <- readxl::read_excel("elsa/G2A ELSA Longitudinal Variable List.xlsx",sheet="biomarkers") %>% 
  rename("selected" = wave2) %>% 
  dplyr::select(level,new_var,selected) %>% 
  dplyr::filter(!is.na(selected)) 

g2aelsa_biomarker_variables_wave4 <- readxl::read_excel("elsa/G2A ELSA Longitudinal Variable List.xlsx",sheet="biomarkers") %>% 
  rename("selected" = wave4) %>% 
  dplyr::select(level,new_var,selected) %>% 
  dplyr::filter(!is.na(selected)) 

g2aelsa_biomarker_variables_wave6 <- readxl::read_excel("elsa/G2A ELSA Longitudinal Variable List.xlsx",sheet="biomarkers") %>% 
  rename("selected" = wave6) %>% 
  dplyr::select(level,new_var,selected) %>% 
  dplyr::filter(!is.na(selected)) 

g2aelsa_biomarker_variables_wave8 <- readxl::read_excel("elsa/G2A ELSA Longitudinal Variable List.xlsx",sheet="biomarkers") %>% 
  rename("selected" = wave8_9) %>% 
  dplyr::select(level,new_var,selected) %>% 
  dplyr::filter(!is.na(selected)) 


wave2 <- read_dta(paste0(path_g2a_longitudinal_folder,"/working/elsa raw/elsa 0 to 9/stata/stata13_se/wave_2_nurse_data_v2.dta"),
                  col_select = na.omit(g2aelsa_biomarker_variables_wave2$selected)) %>% 
  rename_with(~ g2aelsa_biomarker_variables_wave2$new_var[which(g2aelsa_biomarker_variables_wave2$selected == .x)], 
              .cols = g2aelsa_biomarker_variables_wave2$selected) 

wave4 <- read_dta(paste0(path_g2a_longitudinal_folder,"/working/elsa raw/elsa 0 to 9/stata/stata13_se/wave_4_nurse_data.dta"),
                  col_select = na.omit(g2aelsa_biomarker_variables_wave4$selected)) %>% 
  rename_with(~ g2aelsa_biomarker_variables_wave4$new_var[which(g2aelsa_biomarker_variables_wave4$selected == .x)], 
              .cols = g2aelsa_biomarker_variables_wave4$selected) 

wave6 <- read_dta(paste0(path_g2a_longitudinal_folder,"/working/elsa raw/elsa 0 to 9/stata/stata13_se/wave_6_elsa_nurse_data_v2.dta"),
                  col_select = na.omit(g2aelsa_biomarker_variables_wave6$selected)
                  ) %>% 
  rename_with(~ g2aelsa_biomarker_variables_wave6$new_var[which(g2aelsa_biomarker_variables_wave6$selected == .x)], 
              .cols = g2aelsa_biomarker_variables_wave6$selected)  %>% 
  mutate(hba1c = (hba1c/10.929)+2.15)

wave8 <- read_dta(paste0(path_g2a_longitudinal_folder,"/working/elsa raw/elsa 0 to 9/stata/stata13_se/elsa_nurse_w8w9_data_eul.dta"),
                  col_select = na.omit(g2aelsa_biomarker_variables_wave8$selected)) %>% 
  rename_with(~ g2aelsa_biomarker_variables_wave8$new_var[which(g2aelsa_biomarker_variables_wave8$selected == .x)], 
              .cols = g2aelsa_biomarker_variables_wave8$selected) %>% 
  mutate(hba1c = (hba1c/10.929)+2.15)



biomarkers <- bind_rows(
  wave2 %>% mutate(wave = 2, year = 2004),
  wave4 %>% mutate(wave = 4, year = 2008),
  wave6 %>% mutate(wave = 6, year = 2012),
  wave8 %>% mutate(wave = 8, year = 2016)
) %>% 
  arrange(personid,wave) %>% 
  dplyr::select(personid,wave,wavevisit,year,nurwt_n,nurwt_g,nurgroup,bldwt,everything()) %>% 
  mutate(across(cfib:vitd,function(x) case_when(x < 0 ~ NA_real_,
                                                TRUE ~ x)))

saveRDS(biomarkers,paste0(path_g2a_longitudinal_folder,"/working/elsa biomarkers.RDS"))

