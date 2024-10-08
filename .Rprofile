library(tidyverse)
library(survey)
require(haven)
require(srvyr)

if(Sys.info()["user"]=="JVARGH7"){
  path_g2a_data <- "C:/Cloud/OneDrive - Emory University/data/G2Aging"
  path_g2a_longitudinal_folder <- "C:/Cloud/OneDrive - Emory University/Papers/Crossnational Longitudinal Concordance"
}

if(Sys.info()["user"]=="JGUO258"){
  path_g2a_longitudinal_folder <- "C:/Users/JGUO258/OneDrive - Emory/Crossnational Longitudinal Concordance"
}

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

bmi_max <- 60.00
bmi_cutoff <- c(18.50, 25.00, 30.00)
# Non-Asians
female_wc_cutoff = 80 
female_whr_cutoff = 0.80
male_wc_cutoff = 94 
male_whr_cutoff = 0.95


# Asians
# female_wc_cutoff = 80 
# female_whr_cutoff = 0.85
# male_wc_cutoff = 90 
# male_whr_cutoff = 0.9
fpg_cutoff <- 126
rpg_cutoff <- 200
sbp_cutoff <- 130
dbp_cutoff <- 80

fpg_target <- 126
rpg_target <- 180 #Indian DM guidelines
# Indian HTN guidelines (Shah 2020: 130/80 for <= 60y, 140/90 otherwise)
# Indian HTN guidelines (ICMR 2018: 140/90 for <= 80y, 150/90 otherwise)
sbp_target <- c(130,130) 
agebp_cutoff <- 80
dbp_target <- c(80,80)

fpgpre_cutoff <- 100
rpgpre_cutoff <- 140
sbppre_cutoff <- 120
dbppre_cutoff <- 80



add_normalizedweight <- function(dataset_path, h_weight_col, w_weight_col, h_id_col, w_id_col) {
  dataset <- readRDS(dataset_path) %>%
    dplyr::filter(!is.na(!!sym(h_weight_col)), !!sym(h_weight_col) > 0) %>%
    dplyr::filter(!is.na(!!sym(w_weight_col)), !!sym(w_weight_col) > 0) %>%
    arrange(!!sym(h_id_col), !!sym(w_id_col), wave) %>%
    group_by(wave) %>%
    mutate(h_normalizedweight = !!sym(h_weight_col) / sum(!!sym(h_weight_col)),
           w_normalizedweight = !!sym(w_weight_col) / sum(!!sym(w_weight_col))) %>%
    mutate(h_normalizedweight = h_normalizedweight / n(),
           w_normalizedweight = w_normalizedweight / n()) %>%
    ungroup() %>%
    mutate(h_normalizedweight = h_normalizedweight * n(),
           w_normalizedweight = w_normalizedweight * n())
  
  return(dataset)
}
