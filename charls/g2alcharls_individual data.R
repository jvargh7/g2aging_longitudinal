r_male <- haven::read_dta(paste0(path_g2a_data,"/charls/CHARLS H Data 2021/H_CHARLS_D_Data/H_CHARLS_D_Data.dta"),
                          col_select = na.omit(g2acharls_r_variables$selected)) %>% 
  rename_with(.cols = g2acharls_r_variables$selected,
              ~g2acharls_r_variables$new_var[which(g2acharls_r_variables$selected == .x)]) %>% 
  dplyr::filter(gender == 1)

s_male <- haven::read_dta(paste0(path_g2a_data,"/charls/CHARLS H Data 2021/H_CHARLS_D_Data/H_CHARLS_D_Data.dta"),
                          col_select = na.omit(g2acharls_s_variables$selected)) %>% 
  rename_with(~ g2acharls_s_variables$new_var[which(g2acharls_s_variables$selected == .x)], 
              .cols = g2acharls_s_variables$selected) %>% 
  dplyr::filter(gender == 1)

r_female <- haven::read_dta(paste0(path_g2a_data,"/charls/CHARLS H Data 2021/H_CHARLS_D_Data/H_CHARLS_D_Data.dta"),
                            col_select = na.omit(g2acharls_r_variables$selected)) %>% 
  rename_with(~ g2acharls_r_variables$new_var[which(g2acharls_r_variables$selected == .x)], 
              .cols = g2acharls_r_variables$selected) %>% 
  dplyr::filter(gender == 2)

s_female <- haven::read_dta(paste0(path_g2a_data,"/charls/CHARLS H Data 2021/H_CHARLS_D_Data/H_CHARLS_D_Data.dta"),
                            col_select = na.omit(g2acharls_s_variables$selected)) %>% 
  rename_with(~ g2acharls_s_variables$new_var[which(g2acharls_s_variables$selected == .x)], 
              .cols = g2acharls_s_variables$selected) %>% 
  dplyr::filter(gender == 2)

survey_vars <- c("hhid","HOUSEHOLDid_W1","communityid","householdid")
hh_vars <- c("hh_wealth","hh_income","hh_consumption","hh_size")

if("drinksperday" %in% g2acharls_r_variables$new_var){
  r_male <- r_male %>% 
    mutate(drinksperweek = drinksperday*7)
  
  s_male <- s_male %>% 
    mutate(drinksperweek = drinksperday*7)
  
  r_female <- r_female %>% 
    mutate(drinksperweek = drinksperday*7)
  
  s_female <- s_female %>% 
    mutate(drinksperweek = drinksperday*7)
  
  
}

if(! ("lengthmar" %in% g2acharls_r_variables$new_var)){
  r_male <- r_male %>% 
    mutate(lengthmar = NA_real_)
  
  r_female <- r_female %>% 
    mutate(lengthmar = NA_real_)
  
  s_male <- s_male %>% 
    mutate(lengthmar = NA_real_)
  
  s_female <- s_female %>% 
    mutate(lengthmar = NA_real_)
  
}


if(! ("sbp" %in% g2acharls_r_variables$new_var)){
  r_male <- r_male %>% 
    mutate(sbp = NA_real_,
           dbp = NA_real_,
           sbp1 = NA_real_,
           dbp1 = NA_real_,
           sbp2 = NA_real_,
           dbp2 = NA_real_,
           sbp3 = NA_real_,
           dbp3 = NA_real_)
  
  r_female <- r_female %>% 
    mutate(sbp = NA_real_,
           dbp = NA_real_,
           sbp1 = NA_real_,
           dbp1 = NA_real_,
           sbp2 = NA_real_,
           dbp2 = NA_real_,
           sbp3 = NA_real_,
           dbp3 = NA_real_)
  
  s_male <- s_male %>% 
    mutate(sbp = NA_real_,
           dbp = NA_real_,
           sbp1 = NA_real_,
           dbp1 = NA_real_,
           sbp2 = NA_real_,
           dbp2 = NA_real_,
           sbp3 = NA_real_,
           dbp3 = NA_real_)
  
  s_female <- s_female %>% 
    mutate(sbp = NA_real_,
           dbp = NA_real_,
           sbp1 = NA_real_,
           dbp1 = NA_real_,
           sbp2 = NA_real_,
           dbp2 = NA_real_,
           sbp3 = NA_real_,
           dbp3 = NA_real_)
  
}

if(! ("bmi" %in% g2acharls_r_variables$new_var)){
  r_male <- r_male %>% 
    mutate(bmi = NA_real_,
           waistcircumference = NA_real_,
           height = NA_real_,
           weight = NA_real_)
  
  r_female <- r_female %>% 
    mutate(bmi = NA_real_,
           waistcircumference = NA_real_,
           height = NA_real_,
           weight = NA_real_)
  
  s_male <- s_male %>% 
    mutate(bmi = NA_real_,
           waistcircumference = NA_real_,
           height = NA_real_,
           weight = NA_real_)
  
  s_female <- s_female %>% 
    mutate(bmi = NA_real_,
           waistcircumference = NA_real_,
           height = NA_real_,
           weight = NA_real_)
  
}


source("charls/g2alcharls_preprocessing.R")

male <- bind_rows(r_male %>% mutate(type = "Respondent"),
                  s_male %>% mutate(type = "Spouse"))  %>% 
  g2alcharls_preprocessing(.) %>% 
  rename_at(vars(-one_of(c(survey_vars,hh_vars,"coupleid"))),~paste0("h_",.)) 

female <- bind_rows(r_female %>% mutate(type = "Respondent"),
                    s_female %>% mutate(type = "Spouse"))  %>% 
  g2alcharls_preprocessing(.) %>% 
  rename_at(vars(-one_of(c(survey_vars,hh_vars,"coupleid"))),~paste0("w_",.)) 

heterosexual_couples <- left_join(male %>% dplyr::filter(!is.na(coupleid)),
                                  female %>% dplyr::filter(!is.na(coupleid)) %>% dplyr::select(-one_of(survey_vars,hh_vars)),
                                  by="coupleid") %>% 
  # dplyr::filter(h_type == "Respondent",w_type == "Respondent") 
  # No need to run the below lines because every spouse is a respondent in ELSA
  distinct(coupleid,.keep_all=TRUE) %>% 
  dplyr::filter(!h_spouseid %in% c(0,NA_real_), !w_spouseid %in% c(0,NA_real_))



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
                         by=c("h1_spouseid"="h2_id")) %>% 
  dplyr::select(coupleid,h1_id,h1_spouseid,h2_spouseid,one_of(hh_vars),one_of(survey_vars),h1_bmi,h2_bmi,everything()) %>% 
  dplyr::filter(h1_id == h2_spouseid,!is.na(hh_size),!is.na(h1_type),!is.na(h2_type)) %>% 
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
                         by=c("w1_spouseid"="w2_id")) %>% 
  dplyr::select(coupleid,w1_id,w1_spouseid,w2_spouseid,one_of(hh_vars),one_of(survey_vars),w1_bmi,w2_bmi,everything()) %>% 
  dplyr::filter(w1_id == w2_spouseid,!is.na(hh_size),!is.na(w1_type),!is.na(w2_type)) %>% 
  distinct(coupleid,.keep_all = TRUE) %>% 
  dplyr::filter(!w1_spouseid %in% c(0,NA_real_), !w2_spouseid %in% c(0,NA_real_))


require(Hmisc)
hh_quintiles <- bind_rows(heterosexual_couples,
                          msm_couples,
                          wsw_couples)  %>% 
  distinct(hhid,.keep_all=TRUE) %>% 
  dplyr::select(starts_with("hh"),h_indsampleweight) %>% 
  mutate_at(vars(hh_income),
            function(x) cut(x,breaks=c(wtd.quantile(x,weights=.$h_indsampleweight,probs = c(0,0.33,0.67,1.0))),right=TRUE,include.lowest=TRUE,
                            labels=c("Low","Medium","High"))) %>% 
  
  mutate_at(vars(hh_wealth),
            function(x) cut(x,breaks=c(wtd.quantile(x,weights=.$h_indsampleweight,probs = seq(0,1,by=0.2))),right=TRUE,include.lowest=TRUE,
                            labels=c("Lowest","Low","Medium","High","Highest"))) %>% 
  rename_at(vars(hh_wealth),~paste0(.,"quintile")) %>% 
  rename(hh_incometertile = hh_income) 


heterosexual_couples_hh <- heterosexual_couples %>% 
  left_join(hh_quintiles %>% 
              dplyr::select(hhid,contains("ile")),
            by="hhid")  %>% 
  mutate(communityid = zoo::na.locf(communityid)) %>% 
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


