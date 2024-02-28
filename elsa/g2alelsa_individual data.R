r_male <- haven::read_dta(paste0(path_g2a_data,"/elsa/h_elsa_g2.dta"),
                          col_select = na.omit(g2aelsa_r_variables$selected)) %>% 
  rename_with(.cols = g2aelsa_r_variables$selected,
              ~g2aelsa_r_variables$new_var[which(g2aelsa_r_variables$selected == .x)]) %>% 
  dplyr::filter(gender == 1)

s_male <- haven::read_dta(paste0(path_g2a_data,"/elsa/h_elsa_g2.dta"),
                          col_select = na.omit(g2aelsa_s_variables$selected)) %>% 
  rename_with(~ g2aelsa_s_variables$new_var[which(g2aelsa_s_variables$selected == .x)], 
              .cols = g2aelsa_s_variables$selected) %>% 
  dplyr::filter(gender == 1)

r_female <- haven::read_dta(paste0(path_g2a_data,"/elsa/h_elsa_g2.dta"),
                            col_select = na.omit(g2aelsa_r_variables$selected)) %>% 
  rename_with(~ g2aelsa_r_variables$new_var[which(g2aelsa_r_variables$selected == .x)], 
              .cols = g2aelsa_r_variables$selected) %>% 
  dplyr::filter(gender == 2)

s_female <- haven::read_dta(paste0(path_g2a_data,"/elsa/h_elsa_g2.dta"),
                            col_select = na.omit(g2aelsa_s_variables$selected)) %>% 
  rename_with(~ g2aelsa_s_variables$new_var[which(g2aelsa_s_variables$selected == .x)], 
              .cols = g2aelsa_s_variables$selected) %>% 
  dplyr::filter(gender == 2)

survey_vars <- c("strata","psu","hhid")
hh_vars <- c("hh_wealth","hh_income","hh_consumption","hh_size")

if("drinksperday" %in% g2aelsa_r_variables$new_var){
  r_male <- r_male %>% 
    mutate(drinksperweek = drinksperday*7)
  
  s_male <- s_male %>% 
    mutate(drinksperweek = drinksperday*7)
  
  r_female <- r_female %>% 
    mutate(drinksperweek = drinksperday*7)
  
  s_female <- s_female %>% 
    mutate(drinksperweek = drinksperday*7)
  
  
}

source("elsa/g2alelsa_preprocessing.R")

male <- bind_rows(r_male %>% mutate(type = "Respondent"),
                  s_male %>% mutate(type = "Spouse"))  %>% 
  g2alelsa_preprocessing(.) %>% 
  rename_at(vars(-one_of(c(survey_vars,hh_vars,"coupleid"))),~paste0("h_",.)) 

female <- bind_rows(r_female %>% mutate(type = "Respondent"),
                    s_female %>% mutate(type = "Spouse"))  %>% 
  g2alelsa_preprocessing(.) %>% 
  rename_at(vars(-one_of(c(survey_vars,hh_vars,"coupleid"))),~paste0("w_",.)) 

heterosexual_couples <- left_join(male %>% dplyr::filter(!is.na(coupleid)),
                     female %>% dplyr::filter(!is.na(coupleid)) %>% dplyr::select(-one_of(survey_vars,hh_vars)),
                     by="coupleid") %>% 
  # Husband or Wife has to be part of original sample
  dplyr::filter(h_cohort == 1 | w_cohort == 1) %>% 
  # dplyr::filter(h_type == "Respondent",w_type == "Respondent") 
  # No need to run the below lines because every spouse is a respondent in ELSA
  distinct(coupleid,.keep_all=TRUE) %>% 
  dplyr::filter(!h_spouseid %in% c(0,NA_real_), !w_spouseid %in% c(0,NA_real_))


# msm_couples <- left_join(male %>% 
#                            distinct(h_personid,.keep_all=TRUE) %>% 
#                            dplyr::filter(!is.na(coupleid),h_spousegender=="Male") %>% 
#                             rename_at(vars(-one_of(c(survey_vars,hh_vars))),function(x) str_replace(x,"^h_","h1_")),
#                          male %>% 
#                            distinct(h_personid,.keep_all=TRUE) %>% 
#                            dplyr::filter(!is.na(coupleid),h_spousegender=="Male")  %>% 
#                            dplyr::select(-one_of(survey_vars,hh_vars)) %>% 
#                            rename_at(vars(-one_of(c(survey_vars,hh_vars))),function(x) str_replace(x,"^h_","h2_")),
#                          by=c("coupleid")) %>% 
#   dplyr::select(h1_personid,h2_personid,h1_spouseid,h2_spouseid,coupleid,everything()) %>% 
#   dplyr::filter(h1_personid != h2_personid,!is.na(h1_type),!is.na(h2_type)) %>% 
#   distinct(coupleid,.keep_all = TRUE) %>% 
#   dplyr::filter(!h1_spouseid %in% c(0,NA_real_), !h2_spouseid %in% c(0,NA_real_))
# 
# wsw_couples <- left_join(female %>% 
#                            distinct(w_personid,.keep_all=TRUE) %>% 
#                            dplyr::filter(!is.na(coupleid),w_spousegender=="Female") %>% 
#                            rename_at(vars(-one_of(c(survey_vars,hh_vars))),function(x) str_replace(x,"^w_","w1_")),
#                          female %>% 
#                            distinct(w_personid,.keep_all=TRUE) %>% 
#                            dplyr::filter(!is.na(coupleid),w_spousegender=="Female")  %>% 
#                            dplyr::select(-one_of(survey_vars,hh_vars)) %>% 
#                            rename_at(vars(-one_of(c(survey_vars,hh_vars))),function(x) str_replace(x,"^w_","w2_")),
#                          by=c("coupleid")) %>% 
#   dplyr::select(w1_personid,w2_personid,w1_spouseid,w2_spouseid,coupleid,everything()) %>% 
#   dplyr::filter(w1_personid != w2_personid,!is.na(w1_type),!is.na(w2_type)) %>% 
#   distinct(coupleid,.keep_all = TRUE) %>% 
#   dplyr::filter(!w1_spouseid %in% c(0,NA_real_), !w2_spouseid %in% c(0,NA_real_))


msm_couples <- left_join(male %>% 
                           # distinct(coupleid,.keep_all=TRUE) %>% 
                           dplyr::filter(!is.na(h_spouseid),h_gender == "Male",h_spousegender == "Male")  %>% 
                           rename_at(vars(-one_of(c(survey_vars,hh_vars,"coupleid"))),function(x) str_replace(x,"^h_","h1_")) %>% 
                           dplyr::select(contains("id"),everything()),
                         male %>% 
                           # distinct(coupleid,.keep_all=TRUE) %>% 
                           dplyr::filter(!is.na(h_spouseid),h_gender == "Male",h_spousegender=="Male") %>% 
                           rename_at(vars(-one_of(c(survey_vars,hh_vars,"coupleid"))),function(x) str_replace(x,"^h_","h2_")) %>%
                           dplyr::select(-one_of(survey_vars,hh_vars,"coupleid")) %>%
                           dplyr::select(contains("id"),everything()),
                         by=c("h1_spouseid"="h2_personid")) %>% 
  dplyr::select(coupleid,h1_personid,h1_spouseid,h2_spouseid,one_of(hh_vars),one_of(survey_vars),h1_bmi,h2_bmi,everything()) %>% 
  dplyr::filter(h1_personid == h2_spouseid,!is.na(hh_size),!is.na(h1_type),!is.na(h2_type)) %>% 
  distinct(coupleid,.keep_all = TRUE) %>% 
  dplyr::filter(!h1_spouseid %in% c(0,NA_real_), !h2_spouseid %in% c(0,NA_real_))

wsw_couples <- left_join(female %>% 
                           # distinct(coupleid,.keep_all=TRUE) %>% 
                           dplyr::filter(!is.na(w_spouseid),w_gender == "Female",w_spousegender == "Female")  %>% 
                           rename_at(vars(-one_of(c(survey_vars,hh_vars,"coupleid"))),function(x) str_replace(x,"^w_","w1_")) %>% 
                           dplyr::select(contains("idpn"),everything()),
                         female %>% 
                           # distinct(coupleid,.keep_all=TRUE) %>% 
                           dplyr::filter(!is.na(w_spouseid),w_gender == "Female",w_spousegender=="Female") %>% 
                           rename_at(vars(-one_of(c(survey_vars,hh_vars,"coupleid"))),function(x) str_replace(x,"^w_","w2_")) %>%
                           dplyr::select(-one_of(survey_vars,hh_vars,"coupleid")) %>%
                           dplyr::select(contains("id"),everything()),
                         by=c("w1_spouseid"="w2_personid")) %>% 
  dplyr::select(coupleid,w1_personid,w1_spouseid,w2_spouseid,one_of(hh_vars),one_of(survey_vars),w1_bmi,w2_bmi,everything()) %>% 
  dplyr::filter(w1_personid == w2_spouseid,!is.na(hh_size),!is.na(w1_type),!is.na(w2_type)) %>% 
  distinct(coupleid,.keep_all = TRUE) %>% 
  dplyr::filter(!w1_spouseid %in% c(0,NA_real_), !w2_spouseid %in% c(0,NA_real_))


require(Hmisc)
hh_quintiles <- bind_rows(heterosexual_couples,
                          msm_couples,
                          wsw_couples)  %>% 
  distinct(hhid,.keep_all=TRUE) %>% 
  dplyr::select(starts_with("hh"),h_sampleweight) %>% 
  mutate_at(vars(hh_income),
            function(x) cut(x,breaks=c(wtd.quantile(x,weights=.$h_sampleweight,probs = c(0,0.33,0.67,1.0))),right=TRUE,include.lowest=TRUE,
                            labels=c("Low","Medium","High"))) %>% 
  
  mutate_at(vars(hh_wealth,hh_consumption),
            function(x) cut(x,breaks=c(wtd.quantile(x,weights=.$h_sampleweight,probs = seq(0,1,by=0.2))),right=TRUE,include.lowest=TRUE,
                            labels=c("Lowest","Low","Medium","High","Highest"))) %>% 
  rename_at(vars(hh_wealth,hh_consumption),~paste0(.,"quintile")) %>% 
  rename(hh_incometertile = hh_income) 


heterosexual_couples_hh <- heterosexual_couples %>% 
  left_join(hh_quintiles %>% 
              dplyr::select(hhid,contains("ile")),
            by="hhid")  %>% 
  mutate(psu = zoo::na.locf(psu)) %>% 
  # dplyr::select(hhid,h_personid,w_personid,psu,hhweight) %>% View() %>% 
  distinct(coupleid,.keep_all=TRUE) %>% 
  mutate(hh_lengthmar = case_when(!is.na(h_lengthmar) ~ h_lengthmar,
                                  TRUE ~ w_lengthmar),
         hh_lengthmar_ge10 = case_when(!is.na(h_lengthmar_ge10) ~ h_lengthmar_ge10,
                                       TRUE ~ w_lengthmar_ge10)) %>%
  mutate_at(vars(h_height,w_height),function(x) x*100) 

msm_couples_hh <- msm_couples %>% 
  left_join(hh_quintiles %>% 
              dplyr::select(hhid,contains("ile")),
            by="hhid") 

wsw_couples_hh <- wsw_couples %>% 
  left_join(hh_quintiles %>% 
              dplyr::select(hhid,contains("ile")),
            by="hhid") 


