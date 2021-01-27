
p1 <- read_csv("~/Pasteur/tars/input/validation/validation_table_1param.csv")
ranges <- p1 %>% select(starts_with("true")) %>% lapply(range)
names(ranges) %<>% {gsub("true:", "", .)}
ranges <- list()
ranges[["beta1"]] = seq(0.5, 2.5, by = 0.5)
ranges[["beta_factor"]] = seq(0.2, 1, by = 0.2)
ranges[["E_init"]] = c(1, 5, 10, 15, 20, 40)
ranges[["t_init"]] = seq(as.Date("2020-02-13"), by = "week", length.out = 6)
ranges[["t_inflect"]] = seq(as.Date("2020-03-01"), by = "week", length.out = 5)

scales <- p1 %>% select(estimate, scale) %>% unique
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


tab %>% write_csv("~/Pasteur/tars/input/validation/validation_table_2param.csv")







