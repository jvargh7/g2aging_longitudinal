rm(list=ls());gc();source(".Rprofile")

#continuous variable
id_vars = c("coupleid","h_id","h_spouseid","w_id","w_spouseid",
            "h_indsampleweight","w_indsampleweight","h_hhsampleweight","communityid")

continuous_vars <- c(paste0(rep(c("w_","h_"),each=11),
                            c("sbp","dbp","weight","height",
                              "bmi",
                              "age","eduyr","children","lengthmar",
                              "moderate_pa","vigorous_pa")),
                     "hh_size","hh_wealth","hh_income","hh_lengthmar")

proportion_vars <- c(paste0(rep(c("w_","h_"),each=18),
                            c("screened_bp","diagnosed_bp","medication_bp",
                              "fasting","screened_dm","diagnosed_dm","medication_dm",
                              "heavydrinker",
                              "pregnant","employment","retirement","smokeever","smokecurr","alcohol","lengthmar_ge10",
                              "insurance","dm","htn")),"residence","hh_lengthmar_ge10","hh_htn")

grouped_vars <- c("w_education_h","h_education_h",
                  
                  "hh_wealthquintile","hh_consumptionquintile","hh_incometertile",
                  "w_laborforce","w_smoke","h_laborforce","h_smoke"
)

selected_vars_heterosexual <- c(id_vars,continuous_vars,proportion_vars,grouped_vars)

heterosexual_couples_filtering_charls <- function(df_name){
  
  readRDS(paste0(path_g2a_longitudinal_folder,"/working/charls/",df_name,".RDS")) %>% 
    dplyr::select(one_of(selected_vars_heterosexual)) %>% 
  dplyr::filter(h_hhsampleweight > 0)
  # mutate(across(one_of(c("religion","alcohol_days",
  #                        "race","ethnicity")), ~ as.numeric(as.character(.))))
  # dplyr::filter(h_race == 2, w_race == 2) %>%
  # dplyr::filter(h_lengthmar > 2) %>%
  # pivot_longer(cols=matches("^(w_|h_)"),names_pattern = "^(w_|h_)(.*)",names_to = c("sex",".value"))
  
}


heterosexual_couples_wave1 <- heterosexual_couples_filtering_charls("G2A CHARLS Wave 1 heterosexual couples")
heterosexual_couples_wave2 <- heterosexual_couples_filtering_charls("G2A CHARLS Wave 2 heterosexual couples")
heterosexual_couples_wave3 <- heterosexual_couples_filtering_charls("G2A CHARLS Wave 3 heterosexual couples")
heterosexual_couples_wave4 <- heterosexual_couples_filtering_charls("G2A CHARLS Wave 4 heterosexual couples")



couples =  bind_rows(
  heterosexual_couples_wave1 %>% mutate(wave = 1),
  heterosexual_couples_wave2 %>% mutate(wave = 2),
  heterosexual_couples_wave3 %>% mutate(wave = 3),
  heterosexual_couples_wave4 %>% mutate(wave = 4)
)  %>% 
  arrange(h_id,w_id,wave) %>% 
  dplyr::select(w_id,h_id,w_spouseid,h_spouseid,coupleid,h_indsampleweight,w_indsampleweight,communityid,wave,everything()) %>%
  # Measured blood pressure in both partners
  dplyr::filter(!is.na(w_sbp),!is.na(w_dbp),!is.na(h_sbp),!is.na(h_dbp)) %>% 
  
  group_by(h_id,w_id) %>% 
  mutate(seq_waves = 1:n(),
         count_waves = n()) %>% 
  mutate(w_lagged_sbp = case_when(seq_waves == 1 ~ NA_real_,
                                  TRUE ~ dplyr::lag(w_sbp)),
         h_lagged_sbp = case_when(seq_waves == 1 ~ NA_real_,
                                  TRUE ~ dplyr::lag(h_sbp))
  ) %>% 
  ungroup() 


couples %>% 
  dplyr::filter(count_waves > 1) %>% 
  distinct(h_id,w_id) %>% 
  tally()

saveRDS(couples,paste0(path_g2a_longitudinal_folder,"/working/charls couples.RDS"))





