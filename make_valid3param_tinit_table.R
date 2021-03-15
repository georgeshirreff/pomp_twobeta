library(tidyverse)
library(magrittr)
library(lubridate)

tab = expand.grid(list(`true:beta1` = seq(0, 2.5, by = 0.01)
                 , `true:beta2` = -1
                 , `true:t_init` = seq(as.Date("2020-02-15"), as.Date("2020-03-13"), by = 3)
                 , `true:E_init` = c(1, 3, 10)
                 , `true:t_inflect` = as.Date("2020-03-19")))


set.seed(1)

tab %<>% 
  mutate(`true:beta2` = runif(min = 0, max = 0.8, n = n())
         , `start:beta1` = sample(tab[["true:beta1"]])
         , `start:beta2` = runif(n = n())
         , `start:t_init` = sample(x = seq(as.Date("2020-02-15"), as.Date("2020-03-13"), by = 1), size = n(), replace = T)
         , `start:E_init` = `true:E_init`
         , `start:t_inflect` = `true:t_inflect`) %>% 
  mutate(Array = 1:n()) %>% 
  select(Array, everything())


tab %>% write_csv("~/Pasteur/tars/input/validation/validation_table_3param_tinit.csv")









