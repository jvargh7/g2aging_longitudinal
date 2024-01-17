
rand_respondents <- haven::read_dta(paste0(path_g2a_data,"/hrs/randhrs1992_2018v2.dta"),
                               col_select = na.omit(g2ahrs_rand_r_variables$selected)) %>% 
  rename_with(~ g2ahrs_rand_r_variables$new_var[which(g2ahrs_rand_r_variables$selected == .x)], 
              .cols = g2ahrs_rand_r_variables$selected) %>% 
  mutate(hhid = str_sub(hhidpn,1,7),
         pn = as.numeric(pn),
         spouseidpn = case_when(spouseidpn == 0 | is.na(spouseidpn) ~ NA_character_,
                                TRUE ~ sprintf("%09d",spouseidpn))) %>% 
  mutate(spousepn = as.numeric(str_sub(spouseidpn,-2,-1))) %>% 
  dplyr::select(hhid,pn,hhidpn,spousepn,spouseidpn,everything())


rand_spouses <- haven::read_dta(paste0(path_g2a_data,"/hrs/randhrs1992_2018v2.dta"),
                               col_select = na.omit(g2ahrs_rand_s_variables$selected)) %>% 
  rename_with(~ g2ahrs_rand_s_variables$new_var[which(g2ahrs_rand_s_variables$selected == .x)], 
              .cols = g2ahrs_rand_s_variables$selected) %>% 
  dplyr::filter(!is.na(hhidpn),hhidpn > 0)   %>% 
  mutate(# hhid is correct for spouses
         hhid = str_sub(spouseidpn,1,7),
         pn = as.numeric(str_sub(hhidpn,start = -2,end=-1)),
         hhidpn = case_when(hhidpn == 0 | is.na(hhidpn) ~ NA_character_,
                                TRUE ~ sprintf("%09d",as.numeric(paste0(hhid,pn))))) %>%
  mutate(spousepn = as.numeric(str_sub(spouseidpn,-2,-1))) %>% 
  dplyr::select(hhid,pn,hhidpn,spousepn,spouseidpn,everything())


g2a_respondents <- haven::read_dta(paste0(path_g2a_data,"/hrs/H_HRS_c.dta"),
                                    col_select = na.omit(g2ahrs_harmonized_r_variables$selected)) %>% 
  rename_with(~ g2ahrs_harmonized_r_variables$new_var[which(g2ahrs_harmonized_r_variables$selected == .x)], 
              .cols = g2ahrs_harmonized_r_variables$selected) %>% 
  mutate(pn = as.numeric(pn),
         hhid = paste0(hhid,"0"))


g2a_spouses <- haven::read_dta(paste0(path_g2a_data,"/hrs/H_HRS_c.dta"),
                                col_select = na.omit(g2ahrs_harmonized_s_variables$selected)) %>% 
  rename_with(~ g2ahrs_harmonized_s_variables$new_var[which(g2ahrs_harmonized_s_variables$selected == .x)], 
              .cols = g2ahrs_harmonized_s_variables$selected) %>% 
  mutate(pn = as.numeric(pn),
         hhid = paste0(hhid,"0")) 


respondents = left_join(rand_respondents,
                        g2a_respondents,
                        by = c("hhid","pn"))


spouses = left_join(rand_spouses,
                        g2a_spouses,
                        by = c("hhid","spousepn" = "pn"))


survey_vars <- c("strata","psu","hhsampleweight","hhanalysisweight","indsampleweight","pn","hhid","spousepn")
hh_vars <- c("hh_wealth","hh_income","hh_children","hh_size")


male <- bind_rows(respondents %>% dplyr::filter(gender == 1) %>% mutate(type = "Respondent"),
                  spouses %>% dplyr::filter(gender == 1) %>% mutate(type = "Spouse")) %>% 
  mutate(coupleid = paste0(hhid,pmin(pn,spousepn,na.rm = TRUE),"_",pmax(pn,spousepn,na.rm=TRUE))) %>% 
  g2alhrs_preprocessing(.)



female <- bind_rows(respondents %>% dplyr::filter(gender == 2) %>% mutate(type = "Respondent"),
                  spouses %>% dplyr::filter(gender == 2) %>% mutate(type = "Spouse")) %>% 
  mutate(coupleid = paste0(hhid,pmin(pn,spousepn,na.rm = TRUE),"_",pmax(pn,spousepn,na.rm=TRUE))) %>% 
  g2alhrs_preprocessing(.)



heterosexual_couples <- left_join(male %>% 
                                    distinct(coupleid,.keep_all=TRUE) %>% 
                                    dplyr::filter(!is.na(spouseidpn))  %>% 
                                    rename_at(vars(-one_of(c(survey_vars,hh_vars,"coupleid"))),~paste0("h_",.)) %>% 
                                    dplyr::select(contains("idpn"),everything()),
                                  female %>% 
                                    distinct(coupleid,.keep_all=TRUE) %>% 
                                    dplyr::filter(!is.na(spouseidpn)) %>% 
                                    mutate(coupleid = paste0(hhid,pmin(pn,spousepn),"_",pmax(pn,spousepn))) %>% 
                                    rename_at(vars(-one_of(c(survey_vars,hh_vars,"coupleid"))),~paste0("w_",.)) %>% 
                                    dplyr::select(-one_of(survey_vars,hh_vars)) %>% 
                                    dplyr::select(contains("idpn"),everything()),
                                  by=c("coupleid")) %>% 
  dplyr::select(h_hhidpn,h_spouseidpn,w_hhidpn,w_spouseidpn,one_of(hh_vars),one_of(survey_vars),everything()) %>% 
  dplyr::filter(h_hhidpn != h_spouseidpn,!is.na(hh_size),!is.na(w_type),!is.na(h_type)) 


msm_couples <- left_join(male %>% 
                           # distinct(coupleid,.keep_all=TRUE) %>% 
                           dplyr::filter(!is.na(spouseidpn),gender == "Male",spousegender == "Male")  %>% 
                           rename_at(vars(-one_of(c(survey_vars,hh_vars,"coupleid"))),~paste0("h1_",.)) %>% 
                           dplyr::select(contains("idpn"),everything()),
                         male %>% 
                           # distinct(coupleid,.keep_all=TRUE) %>% 
                           dplyr::filter(!is.na(spouseidpn),gender == "Male",spousegender=="Male") %>% 
                           rename_at(vars(-one_of(c(survey_vars,hh_vars,"coupleid"))),~paste0("h2_",.)) %>%
                           dplyr::select(-one_of(survey_vars,hh_vars,"coupleid")) %>%
                           dplyr::select(contains("idpn"),everything()),
                         by=c("h1_spouseidpn"="h2_hhidpn")) %>% 
  dplyr::select(coupleid,h1_hhidpn,h1_spouseidpn,h2_spouseidpn,one_of(hh_vars),one_of(survey_vars),h1_bmi,h2_bmi,everything()) %>% 
  dplyr::filter(h1_hhidpn == h2_spouseidpn,!is.na(hh_size),!is.na(h1_type),!is.na(h2_type)) %>% 
  distinct(coupleid,.keep_all = TRUE)

wsw_couples <- left_join(female %>% 
                           # distinct(coupleid,.keep_all=TRUE) %>% 
                           dplyr::filter(!is.na(spouseidpn),gender=="Female",spousegender == "Female")  %>% 
                           rename_at(vars(-one_of(c(survey_vars,hh_vars,"coupleid"))),~paste0("w1_",.)) %>% 
                           dplyr::select(contains("idpn"),everything()),
                         female %>% 
                           # distinct(coupleid,.keep_all=TRUE) %>% 
                           dplyr::filter(!is.na(spouseidpn),gender=="Female",spousegender=="Female") %>% 
                           rename_at(vars(-one_of(c(survey_vars,hh_vars,"coupleid"))),~paste0("w2_",.)) %>% 
                           dplyr::select(-one_of(survey_vars,hh_vars,"coupleid")) %>% 
                           dplyr::select(contains("idpn"),everything()),
                         by=c("w1_spouseidpn"="w2_hhidpn")) %>% 
  dplyr::select(w1_hhidpn,w1_spouseidpn,w2_spouseidpn,one_of(hh_vars),one_of(survey_vars),w1_bmi,w2_bmi,everything()) %>% 
  dplyr::filter(w1_hhidpn == w2_spouseidpn,!is.na(hh_size),!is.na(w1_type),!is.na(w2_type)) %>% 
  distinct(coupleid,.keep_all = TRUE)



require(Hmisc)
hh_quintiles <- bind_rows(heterosexual_couples,
                          msm_couples,
                          wsw_couples) %>% 
  distinct(hhid,.keep_all=TRUE) %>% 
  dplyr::select(starts_with("hh")) %>% 
  mutate_at(vars(hh_income),
            function(x) cut(x,breaks=c(wtd.quantile(x,weights=.$hhsampleweight,probs = c(0,0.33,0.67,1.0))),right=TRUE,include.lowest=TRUE,
                            labels=c("Low","Medium","High"))) %>% 
  
  mutate_at(vars(hh_wealth),
            function(x) cut(x,breaks=c(wtd.quantile(x,weights=.$hhsampleweight,probs = seq(0,1,by=0.2))),right=TRUE,include.lowest=TRUE,
                            labels=c("Lowest","Low","Medium","High","Highest"))) %>% 
  rename_at(vars(hh_wealth),~paste0(.,"quintile")) %>% 
  rename(hh_incometertile = hh_income)




heterosexual_couples_hh <- heterosexual_couples %>% 
  left_join(hh_quintiles %>% 
              dplyr::select(hhid,contains("ile")),
            by="hhid") 

msm_couples_hh <- msm_couples %>% 
  left_join(hh_quintiles %>% 
              dplyr::select(hhid,contains("ile")),
            by="hhid") 

wsw_couples_hh <- wsw_couples %>% 
  left_join(hh_quintiles %>% 
              dplyr::select(hhid,contains("ile")),
            by="hhid") 