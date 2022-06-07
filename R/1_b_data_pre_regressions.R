#install.packages("pdp", dependencies =TRUE)
#install.packages("ICEbox")

library(tidyverse)

#load data ----
data = read_rds("data/data2.rds")





#### per capita if needed 
# 
# listpercapita = names(data)[c(72:91,146:185)]
# 

# data into tibble -----
new = as_tibble(data)


#####  loop making deaths as per age cohort
# for (i in listpercapita){
#   
#   new[, paste0("D_",i)] = new[, paste0("D_",i)] / new[,paste(i)]
#   
# }

# compute shares of age cohorts

# for total pop
# new[, 72:91] = new[, 72:91] / new[, 71]
# 
# listttotal = names(data)[c(72:91)]
# listttotal = substr(listttotal, 1 , nchar(listttotal)-1)
# 
# 
# #for females
# for (i in listttotal){
#   
#   new[, paste0(i,"F")] = new[, paste0(i,"F")] / new[, "T_F" ]
#   
# }
# 
# #for males
# 
# 
# for (i in listttotal){
#   
#   new[, paste0(i,"M")] = new[, paste0(i,"M")] / new[, "T_M" ]
#   
# }


##### erase double col

## erase those col that are double (indicating by ".x" and ".y" at the end)

new <- new %>% select(!ends_with(".y")) 


#remove the ".x" at the end of a variables name, if it exists.

names(new) <- sub(".x", "", names(new))


#erase those col with NA-rate higher than 25%


# how many NA in the variables

howmanynas <- new %>% 
  pivot_longer(3:length(new),
               names_to ="variable") %>% 
  group_by(variable) %>% 
  summarise(na_rate = sum(is.na(value))/ (length(new) -2)) %>%
  # filter NA higher or equal than 0.25
  filter(na_rate <= 0.25) %>% 
  pull(variable)

new <- new %>% select(all_of(howmanynas))


### old approach, where a lot of variables are included
#exclude variables that are meaningless -----
variablestoexclude <- c(#"STD_MORT", 
                        #"STD_MORT_M", 
                        #"STD_MORT_F", 
                        #"DEATH_RA_T", 
                        #"DEATH_RA_M", 
                        #x"DEATH_RA_F", 
                        #"YOU_DEATH_RA_T", 
                        #"YOU_DEATH_RA_M", 
                        #"YOU_DEATH_RA_F", 
                        "D_Y65_MAX_T", 
                        "D_Y65_MAX_M", 
                        "D_Y65_MAX_F",
                        "D_Y15_64_T",
                        "D_Y15_64_M",
                        "D_Y15_64_F",
                        "D_Y0_14_T",
                        "D_Y0_14_M",
                        "D_Y0_14_F",
                        "INF_MORT_T",
                        "INF_MORT_M",
                        "INF_MORT_F",
                        "Y0_14_T",
                        "Y0_14_M",
                        "Y0_14_F",
                        "POP_Y15_MAX_T",
                        "POP_Y15_MAX_M",
                        "POP_Y15_MAX_F",
                        "EMP_Y15_MAX_T",
                        "EMP_Y15_MAX_M",
                        "EMP_Y15_MAX_F",
                        "LF_Y15_MAX_T",
                        "LF_Y15_MAX_M",
                        "LF_Y15_MAX_F",
                        #"PARTIC_RA_T",
                        #"PARTIC_RA_M",
                        #"PARTIC_RA_F",
                        #"PARTIC_RA_GR_2001_T",
                        #"PARTIC_RA_GR_2007_T",
                        "EDU_LF_T")

#new <- new %>% select(!any_of(variablestoexclude))


#imputing missing values -----
library(missRanger)

#impute using ranger
new_imp <- missRanger(new[1:length(new)], num.trees = 1000)


#feedback imputed data again into original dataset
new<-cbind(data[,1:2],new_imp) %>% as_tibble()

#save dataset to use later one

write_rds(new, "data/processed_data/regression_ready.rds")



