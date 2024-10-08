
colnames(hrs_unique)

#continuous variable
continuous_vars <- c("h_age", "w_age",
                     "h_sbp", "w_sbp",
                     "h_dbp", "w_dbp",
                     "h_height", "w_height",
                     "h_weight", "w_weight",
                     "h_bmi","w_bmi",
                     "h_waistcircumference", "w_waistcircumference",
                     "h_moderate_pa", "w_moderate_pa",
                     "h_vigorous_pa", "w_vigorous_pa",
                     "hh_children", "hh_size", "hh_lengthmar"
                     )

proportion_vars <- c("h_diagnosed_bp", "w_diagnosed_bp",
                     "h_diagnosed_dm", "w_diagnosed_dm",
                     "h_heavydrinker", "w_heavydrinker",
                     "hh_lengthmar_ge10",
                     "w_htn", "h_htn","hh_htn" #add hh_htn here
                     )

#any factor variables
grouped_vars <- c("h_laborforce", "w_laborforce",
                  "h_religion", "w_religion",        
                  "h_smoke", "w_smoke",
                  #"residence",
                  "w_education_h", "h_education_h",
                  "hh_incometertile", "hh_wealthquintile"
                  )

id_vars = c("h_hhidpn","h_hhidpn","h_spouseidpn","w_spouseidpn","strata","psu","wave",
            "hhanalysisweight","hhsampleweight","h_indsampleweight","w_indsampleweight",
            "h_normalizedweight","h_normalizedweight")


#-----------------------------------------------
#multiple imputation
require(survey)
require(mice)


before_imputation <- hrs_unique %>% 
  dplyr::select(one_of(id_vars),
                one_of(continuous_vars), one_of(proportion_vars), one_of(grouped_vars)) %>% 
  mutate_at(vars(w_moderate_pa,w_vigorous_pa,
                 h_moderate_pa,h_vigorous_pa,
                 h_heavydrinker,w_heavydrinker,
                 #residence, 
                 hh_lengthmar
                 ),~as.numeric(.)) %>% #what is this for?
  # Step 1: One hot encoding
  mutate(
    w_education_2 = case_when(w_education_h == "Upper secondary and vocational training" ~ 1,
                              !is.na(w_education_h) ~ 0,
                              TRUE ~ NA_real_),
    w_education_3 = case_when(w_education_h == "Tertiary" ~ 1,
                              !is.na(w_education_h) ~ 0,
                              TRUE ~ NA_real_),
    h_education_2 = case_when(h_education_h == "Upper secondary and vocational training" ~ 1,
                              !is.na(h_education_h) ~ 0,
                              TRUE ~ NA_real_),
    h_education_3 = case_when(h_education_h == "Tertiary" ~ 1,
                              !is.na(h_education_h) ~ 0,
                              TRUE ~ NA_real_),
    
    hh_lengthmar10 = case_when(hh_lengthmar >= 10 ~ 1,
                               TRUE ~ 0),
    
    w_ge65 = case_when(w_age >= 65 ~ 1,
                       TRUE ~ 0),
    h_ge65 = case_when(h_age >= 65 ~ 1,
                       TRUE ~ 0),
    
    hh_low = case_when(hh_wealthquintile == "Low" ~ 1,
                       TRUE ~ 0),
    hh_medium = case_when(hh_wealthquintile == "Medium" ~ 1,
                          TRUE ~ 0),
    hh_high = case_when(hh_wealthquintile == "High" ~ 1,
                        TRUE ~ 0),
    hh_highest = case_when(hh_wealthquintile == "Highest" ~ 1,
                           TRUE ~ 0)
    
    
  ) %>% 
  # Step 2: Modeling interactions
  mutate(
         # w_htn_residence = residence*w_htn,
         # h_htn_residence = residence*h_htn,
         
         # Since exposure is w_htn and effect modifier is husband's education
         w_htn_h_education_2 = h_education_2*w_htn,
         w_htn_h_education_3 = h_education_3*w_htn,
         
         h_htn_w_education_2 = w_education_2*h_htn,
         h_htn_w_education_3 = w_education_3*h_htn,
         
         # Since exposure is w_htn and effect modifier is husband's age category (<65 vs >=65)
         w_htn_h_ge65 = h_ge65*w_htn,
         h_htn_w_ge65 = w_ge65*h_htn,
         
         # Since exposure is w_htn and effect modifier is length of marriage (<10 vs >=10)
         w_htn_hh_lengthmar_ge10 = hh_lengthmar_ge10*w_htn,
         h_htn_hh_lengthmar_ge10 = hh_lengthmar_ge10*h_htn,
         
         # Since exposure is w_htn and effect modifier is household wealth quintile
         w_htn_hh_low = w_htn*hh_low,
         w_htn_hh_medium = w_htn*hh_medium,
         w_htn_hh_high = w_htn*hh_high,
         w_htn_hh_highest = w_htn*hh_highest,
         
         h_htn_hh_low = h_htn*hh_low,
         h_htn_hh_medium = h_htn*hh_medium,
         h_htn_hh_high = h_htn*hh_high,
         h_htn_hh_highest = h_htn*hh_highest
  ) %>% 
  dplyr::select(-w_education_h,-h_education_h,
                -hh_wealthquintile)

interaction_terms <- c(
                       # "w_htn_residence","h_htn_residence",
                       "w_htn_h_education_2","w_htn_h_education_3",
                       "h_htn_w_education_2","h_htn_w_education_3",
                       "w_htn_h_ge65","h_htn_w_ge65",
                       "w_htn_hh_lengthmar_ge10","h_htn_hh_lengthmar_ge10",
                       "w_htn_hh_low","w_htn_hh_medium","w_htn_hh_high","w_htn_hh_highest",
                       "h_htn_hh_low","h_htn_hh_medium","h_htn_hh_high","h_htn_hh_highest"
)




mi_null <- mice(before_imputation,
                maxit = 0)

method = mi_null$method
pred = mi_null$predictorMatrix


method[proportion_vars] <- map(method[proportion_vars],
                               .f=function(x) case_when(x=="" ~ "",
                                                        TRUE ~ "logreg")) %>% unlist()
method[grouped_vars] <- map(method[grouped_vars],
                               .f=function(x) case_when(x=="" ~ "",
                                                        TRUE ~ "polyreg")) %>% unlist()

pred[id_vars,] <- 0
pred[,id_vars] <- 0
method[id_vars] <- ""

# Impute via equation and do not use for imputation , --------
method["hh_lengthmar_ge10"] <- "~I((hh_lengthmar>=10)*1)"
pred["hh_lengthmar_ge10",] <- 0
pred[,"hh_lengthmar_ge10"] <- 0

method["w_ge65"] <- "~I((w_age>=65)*1)"
method["h_ge65"] <- "~I((h_age>=65)*1)"
pred[c("w_ge65","h_ge65"),] <- 0
pred[,c("w_ge65","h_ge65")] <- 0


for(i_t in interaction_terms){
  print(i_t)
  htn_term = str_extract(i_t,"^(w|h)_htn")
  em_term = str_replace(i_t,pattern=paste0(htn_term,"_"),replacement = "")
  method[i_t] = paste0("~I(",htn_term,"*",em_term,")")
  pred[c(htn_term,em_term),i_t] <- 0
}


for(i_t in interaction_terms){
  print(i_t)
  htn_term = str_extract(i_t,"^(w|h)_htn")
  em_term = str_replace(i_t,pattern=paste0(htn_term,"_"),replacement = "")
  method[i_t] = paste0("~I(",htn_term,"*",em_term,")")
  
  # Do not use interaction terms for imputation of the source variables
  pred[c(htn_term,em_term),i_t] <- 0
}


# Takes ~4h
mi_dfs <- mice(before_imputation,
               method = method,
               pred = pred,
               m=10,maxit=50,seed=500)


saveRDS(mi_dfs, paste0(path_g2a_longitudinal_folder,"/working/G2A HRS Couples mi_dfs.RDS"))





