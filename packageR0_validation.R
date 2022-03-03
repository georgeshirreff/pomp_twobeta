library(tidyverse)
library(R0)

relative_date_key <- read_csv("~/tars/input/relative_date_key.csv", col_names = c("dates", "key_old", "relDate")) %>%
  mutate(dates = as.Date(dates, format = "%d/%m/%Y"))


posneg_fpt = read_csv("~/tars/input/posneg_fpt.csv") %>% 
  transmute(dates = Date, I = pos) %>%
  filter(dates >= as.Date("2020-03-01")
         , dates <= as.Date("2020-04-30")) %>% 
  left_join(relative_date_key %>% dplyr::select(dates, relDate)) %>% 
  transmute(dates = relDate, I)


#attack rate method
est.R0.AR(AR = 0.33)
est.R0.AR(incid = 152, pop.size = 459)

#exponential growth method
mGT<-generation.time(val = c(5.8, 1/1.96))
R0::est.R0.EG(posneg_fpt$I, GT = mGT, begin = 12, end = 61)

R0::estimate.R(posneg_fpt$I, GT = mGT
               , methods = c('EG', 
                             # 'ML',
                             # 'AR',
                             'TD',
                             'SB'))
