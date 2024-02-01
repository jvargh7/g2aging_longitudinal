rm(list=ls());gc();source(".Rprofile")

#continuous variable
id_vars = c("coupleid","h_personid","h_spouseid","w_personid","w_spouseid",
            "h_sampleweight","w_sampleweight","strata","psu")

continuous_vars <- c(paste0(rep(c("w_","h_"),each=11),
                            c("sbp","dbp","weight","height",
                              "bmi",
                              "age","eduyr","children","lengthmar",
                              "moderate_pa","vigorous_pa")),
                     "hh_size","hh_wealth","hh_income","hh_consumption","hh_lengthmar")

proportion_vars <- c(paste0(rep(c("w_","h_"),each=18),
                            c("screened_bp","diagnosed_bp","medication_bp",
                              "fasting","screened_dm","diagnosed_dm","medication_dm",
                              "heavydrinker",
                              "pregnant","employment","retirement","smokeever","smokecurr","alcohol","lengthmar_ge10",
                              "insurance","dm","htn")),"residence","hh_lengthmar_ge10","hh_htn")

grouped_vars <- c("w_education_h","h_education_h",
                  "h_race","h_religion",
                  "w_race","w_religion",
                  "hh_wealthquintile","hh_consumptionquintile","hh_incometertile",
                  "w_laborforce","w_smoke","h_laborforce","h_smoke"
)

selected_vars_heterosexual <- c(id_vars,continuous_vars,proportion_vars,grouped_vars)

heterosexual_couples_filtering_elsa <- function(df_name){
  
  readRDS(paste0(path_g2a_longitudinal_folder,"/working/elsa/",df_name,".RDS")) %>% 
    dplyr::select(one_of(selected_vars_heterosexual)) 
    # dplyr::filter(hhsampleweight > 0) %>% 
    # mutate(across(one_of(c("religion","alcohol_days",
    #                        "race","ethnicity")), ~ as.numeric(as.character(.))))
  # dplyr::filter(h_race == 2, w_race == 2) %>%
  # dplyr::filter(h_lengthmar > 2) %>%
  # pivot_longer(cols=matches("^(w_|h_)"),names_pattern = "^(w_|h_)(.*)",names_to = c("sex",".value"))
  
}


heterosexual_couples_wave8 <- heterosexual_couples_filtering_elsa("G2A ELSA Wave 8 heterosexual couples")
heterosexual_couples_wave6 <- heterosexual_couples_filtering_elsa("G2A ELSA Wave 6 heterosexual couples")
heterosexual_couples_wave4 <- heterosexual_couples_filtering_elsa("G2A ELSA Wave 4 heterosexual couples")
heterosexual_couples_wave2 <- heterosexual_couples_filtering_elsa("G2A ELSA Wave 2 heterosexual couples")



couples =  bind_rows(
  heterosexual_couples_wave2 %>% mutate(wave = 2),
  heterosexual_couples_wave4 %>% mutate(wave = 4),
  heterosexual_couples_wave6 %>% mutate(wave = 6),
  heterosexual_couples_wave8 %>% mutate(wave = 8)
)  %>% 
  arrange(h_personid,w_personid,wave) %>% 
  dplyr::select(w_personid,h_personid,w_spouseid,h_spouseid,coupleid,h_sampleweight,w_sampleweight,strata,psu,wave,everything()) %>%
  # Measured blood pressure in both partners
  dplyr::filter(!is.na(w_sbp),!is.na(w_dbp),!is.na(h_sbp),!is.na(h_dbp)) %>% 
  
  group_by(h_personid,w_personid) %>% 
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
  distinct(h_personid,w_personid) %>% 
  tally()

saveRDS(couples,paste0(path_g2a_longitudinal_folder,"/working/elsa couples.RDS"))





