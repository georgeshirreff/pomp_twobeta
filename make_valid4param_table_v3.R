library(tidyverse)
library(magrittr)


p1 <- read_csv("~/Pasteur/tars/input/validation/validation_table_1param.csv")
ranges <- p1 %>% select(starts_with("true")) %>% lapply(range)
names(ranges) %<>% {gsub("true:", "", .)}
ranges <- list()
NVALSPER = 10

set.seed(2)
ranges[["beta1"]] = rep(each = NVALSPER, seq(0.01, 2.5, by = 0.01))
NVALS = length(ranges[["beta1"]])
ranges[["beta_factor"]] = runif(n = NVALS, min = 0.1, max = 1.0)
ranges[["E_init"]] = rexp(n = NVALS, rate = 1/2) + 1
# ranges[["t_init"]] = runif(n = NVALS, min = as.Date("2020-02-13"), max = as.Date("2020-03-13")) %>% as.Date(origin = "1970-01-01")
ranges[["t_init"]] = rep(x = as.Date("2020-03-05"), NVALS) %>% as.Date(origin = "1970-01-01")
ranges[["t_inflect"]] = runif(n = NVALS, min = as.Date("2020-03-01"), max = as.Date("2020-04-06")) %>% as.Date(origin = "1970-01-01")



tab <- tibble(beta1 = ranges[["beta1"]]
       , beta_factor = ranges[["beta_factor"]]
       , E_init = ranges[["E_init"]]
       , t_init = ranges[["t_init"]]
       , t_inflect = ranges[["t_inflect"]]) %>% 
  rename_all(function(x) paste0("true:", x)) %>% 
  mutate(Array = 1:n()) %>% 
  select(Array, everything()) %>% 
  {cbind(., p1[25, ] %>% select(starts_with("start")))}

#set random starting points, which are just the true values randomly resampled
tab[["start:beta1"]] = sample(tab[["true:beta1"]]) 
tab[["start:beta_factor"]] = sample(tab[["true:beta_factor"]]) 
tab[["start:E_init"]] = sample(tab[["true:E_init"]]) 
tab[["start:t_inflect"]] = sample(tab[["true:t_inflect"]]) 

tab %>% write_csv("~/Pasteur/tars/input/validation/validation_table_4paramv3.csv")









