rm(list=ls());gc();source(".Rprofile")

#continuous variable
id_vars = c("h_hhidpn","h_spouseidpn","w_hhidpn","w_spouseidpn",
            "hhsampleweight","hhanalysisweight","strata","psu")

indweight_vars <- c("h_indsampleweight","h_physicalweight",
                    "w_indsampleweight","w_physicalweight")

continuous_vars <- c("h_age", "w_age",
                     "h_sbp", "w_sbp",
                     "h_dbp", "w_dbp",
                     "h_height", "w_height",
                     "h_weight", "w_weight",
                     "h_bmi","w_bmi",
                     "h_waistcircumference", "w_waistcircumference",
                     "h_moderate_pa", "w_moderate_pa",
                     "h_vigorous_pa", "w_vigorous_pa",
                     "hh_size",
                     "hh_children", "hh_size", "hh_lengthmar")

proportion_vars <- c("h_diagnosed_bp", "w_diagnosed_bp",
                     "h_htn_diagnosed","w_htn_diagnosed",
                     "h_htn_treated","w_htn_treated",
                     
                     "h_diagnosed_dm", "w_diagnosed_dm",
                     "h_heavydrinker", "w_heavydrinker",
                     "h_lengthmar","w_lengthmar",
                     "h_lengthmar_ge10","w_lengthmar_ge10",
                     "w_htn", "h_htn","hh_htn")#add hh_htn here

#any factor variables
grouped_vars <- c("h_laborforce", "w_laborforce",
                  "h_religion", "w_religion",        
                  "h_smoke", "w_smoke",
                  "h_alcohol", "w_alcohol",
                  "h_alcohol_days", "w_alcohol_days",
                  "residence",
                  "w_education_h", "h_education_h",
                  "hh_incometertile", "hh_wealthquintile",
                  "h_race", "w_race",
                  "h_ethnicity","w_ethnicity")

selected_vars_heterosexual <- c(id_vars,indweight_vars,continuous_vars,proportion_vars,grouped_vars)

heterosexual_couples_filtering <- function(df_name){
  
  readRDS(paste0(path_g2a_longitudinal_folder,"/working/hrs/",df_name,".RDS")) %>% 
    dplyr::select(one_of(selected_vars_heterosexual))  %>%
    # dplyr::filter(hhsampleweight > 0) %>% 
    mutate(across(one_of(c("religion","alcohol_days",
                           "race","ethnicity")), ~ as.numeric(as.character(.))))
    # dplyr::filter(h_race == 2, w_race == 2) %>%
    # dplyr::filter(h_lengthmar > 2) %>%
    # pivot_longer(cols=matches("^(w_|h_)"),names_pattern = "^(w_|h_)(.*)",names_to = c("sex",".value"))
  
}


heterosexual_couples_wave14 <- heterosexual_couples_filtering("G2A HRS Wave 14 heterosexual couples")
heterosexual_couples_wave13 <- heterosexual_couples_filtering("G2A HRS Wave 13 heterosexual couples")
heterosexual_couples_wave12 <- heterosexual_couples_filtering("G2A HRS Wave 12 heterosexual couples")
heterosexual_couples_wave11 <- heterosexual_couples_filtering("G2A HRS Wave 11 heterosexual couples")
heterosexual_couples_wave10 <- heterosexual_couples_filtering("G2A HRS Wave 10 heterosexual couples")
heterosexual_couples_wave9 <- heterosexual_couples_filtering("G2A HRS Wave 9 heterosexual couples")
heterosexual_couples_wave8 <- heterosexual_couples_filtering("G2A HRS Wave 8 heterosexual couples")




couples =  bind_rows(
  heterosexual_couples_wave8 %>% mutate(wave = 8),
  heterosexual_couples_wave9 %>% mutate(wave = 9),
  heterosexual_couples_wave10 %>% mutate(wave = 10),
  heterosexual_couples_wave11 %>% mutate(wave = 11),
  heterosexual_couples_wave12 %>% mutate(wave = 12),
  heterosexual_couples_wave13 %>% mutate(wave = 13),
  heterosexual_couples_wave14 %>% mutate(wave = 14)
)  %>% 
  arrange(h_hhidpn,w_hhidpn,wave) %>% 
  dplyr::select(h_hhidpn,w_hhidpn,h_spouseidpn,w_spouseidpn,wave,everything()) %>%
  # Measured blood pressure in both partners
  dplyr::filter(!is.na(w_sbp),!is.na(w_dbp),!is.na(h_sbp),!is.na(h_dbp)) %>% 
  
  # Get physical measure weights for individual sample
  mutate(h_physicalweight_imp = case_when(wave %in% c(13,14) ~ h_indsampleweight*2,
                                          is.na(h_physicalweight) ~ h_indsampleweight*2,
                                          TRUE ~ h_physicalweight),
         w_physicalweight_imp = case_when(wave %in% c(13,14) ~ w_indsampleweight*2,
                                          is.na(w_physicalweight) ~ w_indsampleweight*2,
                                          TRUE ~ w_physicalweight)
  ) %>%    
  
  group_by(h_hhidpn,w_hhidpn) %>% 
  mutate(seq_waves = 1:n(),
         count_waves = n()) %>% 
  mutate(w_lagged_sbp = case_when(seq_waves == 1 ~ NA_real_,
                                  TRUE ~ dplyr::lag(w_sbp)),
         h_lagged_sbp = case_when(seq_waves == 1 ~ NA_real_,
                                  TRUE ~ dplyr::lag(h_sbp)),
         
         w_lagged_bmi = case_when(seq_waves == 1 ~ NA_real_,
                                  TRUE ~ dplyr::lag(w_bmi)),
         h_lagged_bmi = case_when(seq_waves == 1 ~ NA_real_,
                                  TRUE ~ dplyr::lag(h_bmi))
  ) %>% 
  ungroup() 
  

couples %>% 
  dplyr::filter(count_waves > 1) %>% 
  distinct(h_hhidpn,w_hhidpn) %>% 
  tally()

saveRDS(couples,paste0(path_g2a_longitudinal_folder,"/working/hrs couples.RDS"))




  
