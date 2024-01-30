rm(list=ls());gc();source(".Rprofile")
source("C:/code/external/functions/preprocessing/dictionary_file.R")

list_files = list.files(paste0(path_g2a_data,"/elsa/elsa 0 to 9/stata/stata13_se"))


dictionaries_compiled <- map_dfr(list_files[44:67],
    function(l_f){
      
      df = haven::read_dta(paste0(path_g2a_data,"/elsa/elsa 0 to 9/stata/stata13_se/",l_f))
      dictionary_file(df,type="dta2",return_dictionary = TRUE,name=l_f) %>% 
        mutate(file = l_f) %>% 
        return()
      
    })


dictionary_file()