library(magrittr)
library(tidyverse)
library(ggplot2)
library(ggpubr)



quantile.date.or <- function(vec, prob){
  if(class(vec) == "Date"){
    num_vec = as.numeric(vec)
    num_vec %>% quantile(prob = prob) %>% as.Date(origin = "1970-01-01")
  } else {
    vec %>% quantile(prob = prob)
  }
}


# experiment_name = "posneg Inflect valid1param v2"
# experiment_name = "posneg Inflect valid1param v3"
# experiment_name = "posneg Inflect valid1param long"
# experiment_name = "posneg Inflect valid2param"
# experiment_name = "posneg Inflect validALLparam"
# experiment_name = "posneg Inflect validALLparam wide"
# experiment_name = "posneg Inflect valid4param"
# experiment_name = "posneg Inflect valid4param fakeextension"
# experiment_name = "posneg Inflect valid4param fakeallover"
# experiment_name = "posneg Inflect valid4param t_inflectivp5"
# experiment_name = "posneg Inflect valid4param startt_inflect"
# experiment_name = "posneg Inflect valid4param t_inflectivp20"
# experiment_name = "posneg Inflect valid4param longhot"
experiment_name = "posneg Inflect valid4param startssampled"

# res <- read_csv(paste0("~/Pasteur/tars/output/TOY/BiasTest_TOY_", experiment_name, ".csv"))



files = list.files(path = "~/Pasteur/tars/output/TOY/", pattern = paste0("*_", experiment_name, "_.*.csv")
                   , full.names = T)


files = files[order(gsub(".*_([0-9]+)[.].*", "\\1", files) %>% as.numeric)] %>% {gsub(".*Documents", "~", .)}
# files = files[!grepl("_trace.csv", files)]

# i = 100
i = 1
for(i in 1:length(files)){
  if(i %% 100 == 0) {print(i);gc()}
  if(i == 1){
    res_read = read_csv(files[i])
    cols = spec(res_read)
  } else {
    res_read = rbind(res_read, read_csv(files[i], col_types = cols))
  }
}
res = res_read
# res <- rbind(res, res_read)
res$`true:beta1` %>% table


# res %>%  write_csv(paste0("~/Pasteur/tars/output/TOY/cat/seirRefresh_", experiment_name, ".csv" ))
res %>%  write_csv(paste0("~/Pasteur/tars/output/TOY/cat/BiasTest_seirInflect_", experiment_name, ".csv" ))









# experiment_name = "posneg Inflect valid1param v2"
experiment_name = "posneg Inflect valid1param v3"
# experiment_name = "posneg Inflect valid1param long"


res <- read_csv(paste0("~/Pasteur/tars/output/TOY/cat/BiasTest_seirInflect_", experiment_name, ".csv")) %>% 
  mutate(across(contains("t_"), function(x) as.Date(x, origin = "1970-01-01")))

# res$`true:beta_factor` %>% table

res <- bind_rows(res
             , res %>% 
               filter(estimate %in% c("beta1", "beta_factor")) %>% 
               mutate(estimate = "beta2") %>% 
               mutate(beta2 = beta1*beta_factor
                      , `start:beta2` = `start:beta1` * `start:beta_factor`
                      , `true:beta2` = `true:beta1` * `true:beta_factor`))

e = "beta1"
e = "beta2"
e = "beta_factor"
e = "E_init"
e = "t_init"
e = "t_inflect"



upper_limits <- list(beta1 = c(NA, 5), beta2 = c(NA, 5), beta_factor = c(NA, 5), E_init = c(NA, NA)
                     , t_init = as.Date(c("2020-02-01", "2020-05-01"))
                     , t_inflect = as.Date(c("2020-02-01", "2020-05-01"))
                     )

pls <- list()

for(e in c("beta1", "beta2", "beta_factor", "E_init", "t_init", "t_inflect")){
  
  pls[[e]] <- res %>% 
    filter(estimate == e) %>% 
    rename(`true:VAR` = paste0("true:", e)) %>% 
    group_by(Array, `true:VAR`) %>% 
    summarise_(`mean:VAR` = paste0("mean(", e, ")")
              , `median:VAR` = paste0("median(", e, ")")
              , `lowci:VAR` = paste0("quantile.date.or(", e, ", prob = 0.025)")
              , `highci:VAR` = paste0("quantile.date.or(", e, ", prob = 0.975)")) %>% 
    ggplot(aes(x = `true:VAR`)) +
    geom_line(aes(y = `median:VAR`, colour = "Median"), size = 1) + 
    geom_line(aes(y = `mean:VAR`, colour = "Mean"), size = 1) +  
    geom_abline(slope = 1, intercept = 0) + 
    geom_ribbon(aes(x = `true:VAR`, ymin = `lowci:VAR`, ymax = `highci:VAR`), colour = "grey", alpha = 0.2) + 
    labs(title = "", colour = "", x = paste("true", e), y = paste("estimate", e))  + 
    coord_cartesian(ylim = upper_limits[[e]]) +
    theme_bw() + 
    theme(text = element_text(size = 20), legend.position = c(0.2, 0.8)
          , legend.background = element_rect(fill="white", linetype="solid"))

}


ggpubr::ggarrange(plotlist = pls)
ggsave(paste0("~/Pasteur/tars/output/Inflection/", "validation_", experiment_name, ".png"), units = "cm", width = 50, height = 30)






# 2param

experiment_name = "posneg Inflect valid2param"

res <- read_csv(paste0("~/Pasteur/tars/output/TOY/cat/BiasTest_seirInflect_", experiment_name, ".csv")) %>% 
  mutate(across(contains("t_"), function(x) as.Date(x, origin = "1970-01-01")))

params = c("beta1", "beta_factor", "E_init", "t_init", "t_inflect")

double_pls <- list()
i = 1
j = 2
for(i in 1:4){
  for(j in (i+1):5){
    params[i]
    params[j]
    
    double_pls[[paste0(params[i], "&", params[j])]] <- res %>% 
      filter(estimate1 == params[i], estimate2 == params[j]) %>% 
      rename(`true:VAR1` = paste0("true:", params[i])
             , `true:VAR2` = paste0("true:", params[j])) %>% 
      group_by(Array, `true:VAR1`, `true:VAR2`) %>% 
      summarise_(`mean:VAR1` = paste0("mean(", params[i], ")")
                 , `mean:VAR2` = paste0("mean(", params[j], ")")
                 , `median:VAR1` = paste0("median(", params[i], ")")
                 , `median:VAR2` = paste0("median(", params[j], ")")) %>% 
      ggplot(aes(x = `true:VAR1`, y = `true:VAR2`)) + geom_point() + 
      geom_segment(aes(xend = `median:VAR1`, yend = `median:VAR2`), arrow = arrow(length = unit(0.3, "cm"))) +
      theme_bw()+
      labs(x = params[i], y = params[j]) +
      theme(text = element_text(size = 20))
      
                 # , `lowci:VAR` = paste0("quantile.date.or(", e, ", prob = 0.025)")
                 # , `highci:VAR` = paste0("quantile.date.or(", e, ", prob = 0.975)"))
    
  }
}

ggpubr::ggarrange(plotlist = double_pls)

ggsave(paste0("~/Pasteur/tars/output/Inflection/", "validation_", experiment_name, ".png"), units = "cm", width = 50, height = 30)




# ALL param

# experiment_name = "posneg Inflect validALLparam"
# experiment_name = "posneg Inflect validALLparam wide"
# experiment_name = "posneg Inflect valid4param"
# experiment_name = "posneg Inflect valid4param fakeextension"
# experiment_name = "posneg Inflect valid4param fakeallover"
# experiment_name = "posneg Inflect valid4param t_inflectivp5"
# experiment_name = "posneg Inflect valid4param startt_inflect"
# experiment_name = "posneg Inflect valid4param t_inflectivp20"
# experiment_name = "posneg Inflect valid4param longhot"
experiment_name = "posneg Inflect valid4param startssampled"



res <- read_csv(paste0("~/Pasteur/tars/output/TOY/cat/BiasTest_seirInflect_", experiment_name, ".csv")) %>% 
  mutate(across(contains("t_"), function(x) as.Date(x, origin = "1970-01-01")))

res %>% 
  ggplot(aes(x = `true:t_inflect`, y = t_inflect)) + geom_point()

res %>% 
  ggplot(aes(x = t_inflect)) + geom_histogram() + 
  facet_wrap(.~`true:t_inflect`) + 
  geom_vline(aes(xintercept = `true:t_inflect`, colour = "true")) + 
  geom_vline(aes(xintercept = as.Date("2020-03-23"), colour = "Mar23"))
  
# valid_table <- read_csv("~/Pasteur/tars/input/validation/validation_table_4paramv2.csv")
# valid_table %>% 
#   ggplot(aes(x = `start:t_inflect`, y = `true:t_inflect`)) + geom_point()

# valid_table <- read_csv("~/Pasteur/tars/input/validation/validation_table_4param.csv")
# 
# valid_table$worked = valid_table$Array %in% res$Array
# valid_table %>% 
#   ggplot(aes(x = `true:E_init`, fill = worked)) + geom_histogram()
# 
# valid_table %>% 
#   arrange(`true:E_init`) %>% 
#   mutate(order = 1:n()) %>% 
#   ggplot(aes(x = order, y = `true:E_init`, colour = worked)) + geom_point() + 
#   facet_wrap(.~worked) + 
#   theme(panel.grid.major.y = 0:25) + 
#   theme_bw()



res <- res %>% 
  mutate(beta2 = beta1*beta_factor
         , `start:beta2` = `start:beta1` * `start:beta_factor`
         , `true:beta2` = `true:beta1` * `true:beta_factor`)



e = "beta1"
e = "beta2"
e = "beta_factor"
e = "E_init"
e = "t_init"
e = "t_inflect"



upper_limits <- list(beta1 = c(NA, 5), beta2 = c(NA, 5), beta_factor = c(NA, 5), E_init = c(NA, 100)
                     , t_init = as.Date(c("2020-02-01", "2020-05-01"))
                     , t_inflect = as.Date(c("2020-02-01", "2020-05-01"))
)

pls <- list()
e = "beta_factor"
for(e in c("beta1", "beta2", "beta_factor", "E_init", "t_init", "t_inflect")){
  
  pls[[e]] <-  res %>% 
    rename(`true:VAR` = paste0("true:", e)) %>% 
    group_by(`true:VAR`) %>% 
    summarise_(`mean:VAR` = paste0("mean(", e, ")")
               , `median:VAR` = paste0("median(", e, ")")
               , `lowci:VAR` = paste0("quantile.date.or(", e, ", prob = 0.025)")
               , `highci:VAR` = paste0("quantile.date.or(", e, ", prob = 0.975)")) %>% 
    ggplot(aes(x = `true:VAR`)) +
    geom_line(aes(y = `median:VAR`, colour = "Median"), size = 1) + 
    geom_line(aes(y = `mean:VAR`, colour = "Mean"), size = 1) +
    geom_abline(slope = 1, intercept = 0) + 
    # geom_ribbon(aes(x = `true:VAR`, ymin = `lowci:VAR`, ymax = `highci:VAR`), colour = "grey", alpha = 0.2) +
    labs(title = "", colour = "", x = paste("true", e), y = paste("estimate", e))  + 
    coord_cartesian(ylim = upper_limits[[e]]) +
    theme_bw() + 
    theme(text = element_text(size = 20), legend.position = c(0.2, 0.8)
          , legend.background = element_rect(fill="white", linetype="solid"))
  
}



ggpubr::ggarrange(plotlist = pls)

ggsave(paste0("~/Pasteur/tars/output/Inflection/", "validation_", experiment_name, ".png"), units = "cm", width = 50, height = 30)
