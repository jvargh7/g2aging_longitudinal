rm(list=ls());gc();source(".Rprofile")
source("C:/code/external/functions/preprocessing/dictionary_file.R")

list_files = list.files(paste0(path_g2a_longitudinal_folder,"/working/elsi raw"))


dictionaries_compiled <- map_dfr(list_files,
                                 function(l_f){
                                   
                                   df = haven::read_dta(paste0(path_g2a_longitudinal_folder,"/working/elsi raw/",l_f))
                                   dictionary_file(df,type="dta2",return_dictionary = TRUE,name=l_f) %>% 
                                     mutate(file = l_f) %>% 
                                     return()
                                   
                                 })


dictionary_file()