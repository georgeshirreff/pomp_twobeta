library(tidyverse)
library(magrittr)


p1 <- read_csv("~/Pasteur/tars/input/validation/validation_table_1param.csv")
ranges <- p1 %>% select(starts_with("true")) %>% lapply(range)
names(ranges) %<>% {gsub("true:", "", .)}
ranges <- list()
NVALSPER = 10

set.seed(1)
ranges[["beta1"]] = rep(each = NVALSPER, seq(0.01, 2.5, by = 0.01))
NVALS = length(ranges[["beta1"]])
ranges[["beta_factor"]] = runif(n = NVALS, min = 0.1, max = 1.2)
ranges[["E_init"]] = rexp(n = NVALS, rate = 1/2)
ranges[["t_init"]] = runif(n = NVALS, min = as.Date("2020-02-13"), max = as.Date("2020-03-20")) %>% as.Date(origin = "1970-01-01")
ranges[["t_inflect"]] = runif(n = NVALS, min = as.Date("2020-03-01"), max = as.Date("2020-04-13")) %>% as.Date(origin = "1970-01-01")


tab <- tibble(beta1 = ranges[["beta1"]]
       , beta_factor = ranges[["beta_factor"]]
       , E_init = ranges[["E_init"]]
       , t_init = ranges[["t_init"]]
       , t_inflect = ranges[["t_inflect"]]) %>% 
  rename_all(function(x) paste0("true:", x)) %>% 
  mutate(Array = 1:n()) %>% 
  select(Array, everything()) %>% 
  {cbind(., p1[25, ] %>% select(starts_with("start")))}

tab %>% write_csv("~/Pasteur/tars/input/validation/validation_table_ALLparam.csv")


orig_line <- p1[25, ] %>% 
  select(-estimate, -scale) %>% 
  mutate(Array = -1, estimate1 = -1, estimate2 = -1
         # , scale1 = -1, scale2 = -1
         ) %>%
  select(Array, starts_with("estimate"), everything())


i = 1
j = 2
for(i in 1:4){
  for(j in (i+1):5){
    eg <- expand.grid(ranges[c(i, j)]) %>% 
      rename_all(function(x) paste0("true:", x)) %>% 
      mutate(estimate1 = names(ranges)[i]
             , estimate2 = names(ranges)[j])
    
    tab_piece = eg %>% mutate(Array = -1) %>% left_join(orig_line %>% 
                       select(-names(eg))
    )
    if(i == 1 & j == 2){
      tab = tab_piece
    } else {
      tab = rbind(tab, tab_piece)
    }
    
  }
}

tab <- tab %>% 
  left_join(scales %>% rename_all(~paste0(.x, "1"))) %>% 
  left_join(scales %>% rename_all(~paste0(.x, "2"))) %>% 
  select(Array, starts_with("estimate"), starts_with("scale"), everything()) %>% 
  mutate(`start:E_init` = ifelse(estimate1 == "E_init" | estimate2 == "E_init" 
                                 , 10, `start:E_init`)) %>% 
  mutate(Array = 1:n())









