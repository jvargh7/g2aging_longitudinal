gc();rm(list=ls());source(".Rprofile")


library(haven)

g2aelsa_biomarker_variables_wave1_biomarkers <- readxl::read_excel("charls/G2A CHARLS Longitudinal Variable List.xlsx",sheet="biomarkers") %>% 
  rename("selected" = wave1,
         "dataset" = dataset_wave1) %>% 
  dplyr::filter(dataset == "biomarkers.dta"| is.na(dataset)) %>% 
  dplyr::select(level,new_var,selected) %>% 
  dplyr::filter(!is.na(selected),!is.na(new_var)) 

g2aelsa_biomarker_variables_wave1_blood <- readxl::read_excel("charls/G2A CHARLS Longitudinal Variable List.xlsx",sheet="biomarkers") %>% 
  rename("selected" = wave1,
         "dataset" = dataset_wave1) %>% 
  dplyr::filter(dataset == "Blood_20140429.dta" | is.na(dataset)) %>% 
  dplyr::select(level,new_var,selected) %>% 
  dplyr::filter(!is.na(selected),!is.na(new_var)) 

g2aelsa_biomarker_variables_wave3_biomarkers <- readxl::read_excel("charls/G2A CHARLS Longitudinal Variable List.xlsx",sheet="biomarkers") %>% 
  rename("selected" = wave3,
         "dataset" = dataset_wave3) %>% 
  dplyr::filter(dataset == "Biomarker.dta"| is.na(dataset)) %>% 
  dplyr::select(level,new_var,selected) %>% 
  dplyr::filter(!is.na(selected),!is.na(new_var)) 

g2aelsa_biomarker_variables_wave3_blood <- readxl::read_excel("charls/G2A CHARLS Longitudinal Variable List.xlsx",sheet="biomarkers") %>% 
  rename("selected" = wave3,
         "dataset" = dataset_wave3) %>% 
  dplyr::filter(dataset == "Blood.dta"| is.na(dataset)) %>% 
  dplyr::select(level,new_var,selected) %>% 
  dplyr::filter(!is.na(selected),!is.na(new_var)) 


wave1_biomarkers <- read_dta(paste0(path_g2a_longitudinal_folder,"/working/charls raw/CHARLS 2011/biomarkers.dta"),
                  col_select = na.omit(g2aelsa_biomarker_variables_wave1_biomarkers$selected)) %>% 
  rename_with(~ g2aelsa_biomarker_variables_wave1_biomarkers$new_var[which(g2aelsa_biomarker_variables_wave1_biomarkers$selected == .x)], 
              .cols = g2aelsa_biomarker_variables_wave1_biomarkers$selected)


wave1_blood <- read_dta(paste0(path_g2a_longitudinal_folder,"/working/charls raw/CHARLS 2011/Blood_20140429.dta"),
                             col_select = na.omit(g2aelsa_biomarker_variables_wave1_blood$selected)) %>% 
  rename_with(~ g2aelsa_biomarker_variables_wave1_blood$new_var[which(g2aelsa_biomarker_variables_wave1_blood$selected == .x)], 
              .cols = g2aelsa_biomarker_variables_wave1_blood$selected)



wave3_biomarkers <- read_dta(paste0(path_g2a_longitudinal_folder,"/working/charls raw/CHARLS 2015/Biomarker.dta"),
                             col_select = na.omit(g2aelsa_biomarker_variables_wave3_biomarkers$selected)) %>% 
  rename_with(~ g2aelsa_biomarker_variables_wave3_biomarkers$new_var[which(g2aelsa_biomarker_variables_wave3_biomarkers$selected == .x)], 
              .cols = g2aelsa_biomarker_variables_wave3_biomarkers$selected)


wave3_blood <- read_dta(paste0(path_g2a_longitudinal_folder,"/working/charls raw/CHARLS 2015/Blood/Blood.dta"),
                        col_select = na.omit(g2aelsa_biomarker_variables_wave3_blood$selected)) %>% 
  rename_with(~ g2aelsa_biomarker_variables_wave3_blood$new_var[which(g2aelsa_biomarker_variables_wave3_blood$selected == .x)], 
              .cols = g2aelsa_biomarker_variables_wave3_blood$selected)



biomarkers <- bind_rows(
  wave1_biomarkers %>% full_join(wave1_blood,by="ID") %>% mutate(wave = 1, year = 2011),
  wave3_biomarkers %>% full_join(wave3_blood,by="ID")%>%  mutate(wave = 3, year = 2015)
) %>% 
  arrange(ID,wave) %>% 
  dplyr::select(ID,wave,hhid, communityid, year,blood_weight,everything()) 

saveRDS(biomarkers,paste0(path_g2a_longitudinal_folder,"/working/charls biomarkers.RDS"))
