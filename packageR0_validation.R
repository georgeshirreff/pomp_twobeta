library(tidyverse)
library(R0)
library(magrittr)

relative_date_key <- read_csv("~/tars/input/relative_date_key.csv", col_names = c("dates", "key_old", "relDate")) %>%
  mutate(dates = as.Date(dates, format = "%d/%m/%Y"))


posneg_fpt = read_csv("~/tars/input/posneg_fpt.csv") %>% 
  transmute(dates = Date, I = pos) %>%
  filter(dates >= as.Date("2020-03-01")
         , dates <= as.Date("2020-04-30")) %>% 
  left_join(relative_date_key %>% dplyr::select(dates, relDate)) %>% 
  transmute(dates = relDate, I)




#attack rate method

posneg <- read_csv(paste0("~/tars/input/posneg_alltests.csv")) %>% 
  mutate(Date = as.numeric(Date), tests = neg + pos) %>% 
  filter(Date >= as.numeric(as.Date("2020-03-01"))
         , Date <= as.numeric(as.Date("2020-06-22")))

posnegWard = NULL
for(building in c("A", "B", "C")){
  for(floor in 0:3){
    this_file = paste0("~/tars/input/posneg_alltests_", building, "_", floor, ".csv")
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

SAR_numer <- read_csv("~/tars/input/SAR_numer.csv")
                      
SAR_denom <- posnegWard %>% 
  group_by(wardCode) %>% 
  summarise(max_Patients = max(Patients)
            , total_Patients = Patients[1] + sum(adm)) %>% 
  rbind(posneg %>% 
          summarise(max_Patients = max(Patients)
                    , total_Patients = Patients[1] + sum(adm)) %>% 
          mutate(wardCode = "Whole\nhospital")
  )

SARwards <- SAR_numer %>% 
  left_join(SAR_denom) %>% 
  filter(total_Patients > 0) %>% 
  mutate(SAR = patients_positive/total_Patients) %>% 
  mutate(R0_SAR = sapply(SAR, function(x) est.R0.AR(AR = x) %>% unlist %>% {.["R"]} %>% as.numeric)
         , R0_SAR2 = -log(1-SAR)/SAR)


est.R0.AR(AR = 0.33, )
est.R0.AR(incid = 152, pop.size = 459)

#exponential growth method
mGT<-generation.time(type = "gamma", val = c(5.8, 1/1.96))
# mGT<-generation.time(type = "gamma", val = c(5.2, 1.73))
R0::est.R0.EG(posneg_fpt$I, GT = mGT, begin = 12, end = 61)


# maximum likelihood method
R0::est.R0.ML(posneg_fpt$I, GT = mGT, begin = 12, end = 61)


