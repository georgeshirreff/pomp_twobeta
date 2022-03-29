library(tidyverse)
library(readxl)
library(magrittr)
library(lubridate)
library(gridExtra)
library(pomp)
library(EpiEstim)
library(incidence)

figures_folder = "Figures/"
tables_folder = "Tables/"

posneg <- read_csv(paste0("Data/posneg_alltests.csv")) %>% 
  mutate(Date = as.numeric(Date), tests = neg + pos) %>% 
  filter(Date >= as.numeric(as.Date("2020-03-01"))
         , Date <= as.numeric(as.Date("2020-06-22")))

posnegWard = NULL
for(building in c("A", "B", "C")){
  for(floor in 0:3){
    this_file = paste0("Data/posneg_alltests_", building, "_", floor, ".csv")
    if(file.exists(this_file)){
      posnegWard_piece = read_csv(this_file) %>% 
        mutate(Building = building, Floor = floor)
      
    } else {
      posnegWard_piece = posnegWard_piece %>% 
        mutate(neg = 0, pos = 0, Patients = 0, adm = 0, dd = 0) %>% 
        mutate(Building = building, Floor = floor)
      
    }
    
    if(is.null(posnegWard)){
      posnegWard = posnegWard_piece
    } else {
      posnegWard = rbind(posnegWard
                         , posnegWard_piece)
    }
    
    
    
  }
}

posnegWard %<>% mutate(wardCode = paste0(Building, Floor)
                       , Floor = factor(Floor, levels = 3:0))

SAR_numer <- read_csv("Data/SAR_numer.csv")

SAR_denom <- posnegWard %>% 
  group_by(wardCode) %>% 
  summarise(max_Patients = max(Patients)
            , total_Patients = Patients[1] + sum(adm)) %>% 
  rbind(posneg %>% 
          summarise(max_Patients = max(Patients)
                    , total_Patients = Patients[1] + sum(adm)) %>% 
          mutate(wardCode = "Whole\nhospital")
  )

SAR_numer %>% 
  left_join(SAR_denom) %>% 
  filter(total_Patients > 0) %>% 
  mutate(SAR = patients_positive/total_Patients) %>% 
  mutate(R0_SAR = -log(1 - SAR)/SAR)
