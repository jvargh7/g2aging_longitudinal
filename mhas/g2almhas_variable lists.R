rm(list=ls());gc();source(".Rprofile")
source("C:/code/external/functions/preprocessing/dictionary_file.R")

list_files = list.files(paste0(path_g2a_longitudinal_folder,"/working/mhas raw"))
list_files = list_files[str_detect(list_files,"\\.dta")]


dictionaries_compiled <- map_dfr(list_files,
                                 function(l_f){
                                   
                                   df = haven::read_dta(paste0(path_g2a_longitudinal_folder,"/working/mhas raw/",l_f))
                                   dictionary_file(df,type="dta2",return_dictionary = TRUE,name=l_f) %>% 
                                     mutate(file = l_f) %>% 
                                     return()
                                   
                                 })


list_files2 = list.files(paste0(path_g2a_longitudinal_folder,"/working/mhas raw/MHAS Core/2012"))
list_files2 = list_files2[str_detect(list_files2,"\\.dta")]


dictionaries_compiled <- map_dfr(list_files2,
                                 function(l_f){
                                   
                                   df = haven::read_dta(paste0(path_g2a_longitudinal_folder,"/working/mhas raw/MHAS Core/2012/",l_f))
                                   dictionary_file(df,type="dta2",return_dictionary = TRUE,name=l_f) %>% 
                                     mutate(file = l_f) %>% 
                                     return()
                                   
                                 })
