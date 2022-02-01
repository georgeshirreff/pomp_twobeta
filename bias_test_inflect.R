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
# experiment_name = "posneg Inflect valid4param startssampled"
# experiment_name = "posneg Inflect valid3param"
# experiment_name = "posneg Inflect valid3param beta2"
# experiment_name = "posneg Inflect valid3param longcoldbeta2"
# experiment_name = "posneg Inflect valid3param hotbeta2"
# experiment_name = "posneg Inflect valid3param tinit extinction_threshold0"
experiment_name = "posneg Inflect valid3param tinit extinction_threshold0 tinflect23"


# res <- read_csv(paste0("~/Pasteur/tars/output/TOY/BiasTest_TOY_", experiment_name, ".csv"))



files = list.files(path = "~//output/TOY/output", pattern = paste0("*_", experiment_name, "_.*.csv")
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
res$`true:beta2` %>% table


# res %>%  write_csv(paste0("~/Pasteur/tars/output/TOY/cat/seirRefresh_", experiment_name, ".csv" ))
res %>%  write_csv(paste0("~/Pasteur/tars/output/TOY/cat/BiasTest_seirInflect_", experiment_name, ".csv" ))









# experiment_name = "posneg Inflect valid1param v2"
# experiment_name = "posneg Inflect valid1param v3"
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

res <- read_csv(paste0("~/tars/output/TOY/cat/BiasTest_seirInflect_", experiment_name, ".csv")) %>% 
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
# experiment_name = "posneg Inflect valid4param startssampled"
# experiment_name = "posneg Inflect valid3param"
# experiment_name = "posneg Inflect valid3param beta2"
# experiment_name = "posneg Inflect valid3param longcoldbeta2"
# experiment_name = "posneg Inflect valid3param hotbeta2"
# experiment_name = "posneg Inflect valid3param tinit extinction_threshold0"
experiment_name = "posneg Inflect valid3param tinit extinction_threshold0 tinflect23"

res <- read_delim(paste0("~/tars/output/TOY/cat/BiasTest_seirInflect_", experiment_name, ".csv"), delim = ";") %>% 
  # mutate(across(contains("t_"), function(x) as.Date(x, origin = "1970-01-01")))
mutate(across(contains("t_"), function(x) as.numeric(x - as.numeric(as.Date("2020-03-11")))))


# e = "beta1"
# e = "beta2"
# e = "beta_factor"
# e = "E_init"
# e = "t_init"
# e = "t_inflect"



upper_limits <- list(beta1 = c(NA, 5), beta2 = c(NA, 5), beta_factor = c(NA, 5), E_init = c(0.1, 100)
                     # , t_init = as.Date(c("2020-02-01", "2020-05-01"))
                     # , t_inflect = as.Date(c("2020-02-01", "2020-05-01"))
                     , t_init = c(-39, 51)
                     , t_inflect = c(-39, 51)
)

pls <- list()
e = "beta2"
true_labels = c(beta1 = expression(true~beta[1])
                , beta2 = expression(true~beta[2])
                , E_init = expression(true~E[init])
                , t_init = expression(true~t[init]~relative)
                , t_inflecgt = expression(true~t[inflect]~relative)
                )
est_labels = c(beta1 = expression(estimate~beta[1])
               , beta2 = expression(estimate~beta[2])
               , E_init = expression(estimate~E[init])
               , t_init = expression(estimate~t[init]~relative)
               , t_inflect = expression(estimate~t[inflect]~relative)
               )

e = 't_init'
for(e in c("beta1", "beta2"
           # , "beta_factor"
           , "t_init"
           # , "E_init", "t_inflect"
           )){
  
  pls[[e]] <-  res %>% 
    rename(`true:VAR` = paste0("true:", e)) %>% 
    group_by(`true:VAR`) %>% 
    summarise_(`mean:VAR` = paste0("mean(", e, ")")
               , `median:VAR` = paste0("median(", e, ")")
               , `lowci:VAR` = paste0("quantile.date.or(", e, ", prob = 0.025)")
               , `highci:VAR` = paste0("quantile.date.or(", e, ", prob = 0.975)")) %>% 
    ggplot(aes(x = `true:VAR`)) +
    geom_point(aes(y = `median:VAR`), size = 1, alpha = 0.2) +
    # geom_point(aes(y = `mean:VAR`, colour = "Mean"), size = 1) +
    geom_abline(slope = 1, intercept = 0) + 
    # geom_ribbon(aes(x = `true:VAR`, ymin = `lowci:VAR`, ymax = `highci:VAR`), colour = "grey", alpha = 0.2) +
    labs(title = "", colour = "", x = true_labels[e], y = est_labels[e])  + 
    coord_cartesian(ylim = upper_limits[[e]]) +
    guides(color = F) + 
    theme_bw() #+ 
    # theme(text = element_text(size = 20), legend.position = c(0.2, 0.8)
    #       , legend.background = element_rect(fill="white", linetype="solid"))
  
}


# pls[["E_init"]] = pls[["E_init"]] + scale_x_log10() + scale_y_log10() + coord_cartesian(ylim = c(1, 100))
ggpubr::ggarrange(plotlist = pls, ncol = 3)

# ggsave(paste0("~/tars/output/Figs/", "validation_", experiment_name, ".png"), units = "cm", width = 30, height = 12)

ggsave(paste0("~/tars/output/Figs/", "validation_", experiment_name, "_relative.png"), units = "cm", width = 30, height = 12)

ggsave(pls[["beta1"]]
       , filename = paste0("~/tars/output/Figs/", "SFig5A.jpg"), units = "cm", width = 7, height = 7, device = "jpeg", dpi = 600)
ggsave(pls[["beta2"]]
       , filename = paste0("~/tars/output/Figs/", "SFig5B.jpg"), units = "cm", width = 7, height = 7, device = "jpeg", dpi = 600)
ggsave(pls[["t_init"]]
       , filename = paste0("~/tars/output/Figs/", "SFig5C.jpg"), units = "cm", width = 7, height = 7, device = "jpeg", dpi = 600)



experiment_name = "posneg Inflect valid3param beta2"
experiment_name = "posneg Inflect valid3param longcoldbeta2"


res <- read_delim(paste0("~/tars/output/TOY/cat/BiasTest_seirInflect_", experiment_name, ".csv"), delim = ";") %>% 
  mutate(across(contains("t_"), function(x) as.Date(x, origin = "1970-01-01")))


res %>% transmute(diff = beta2-`true:beta2`) %>% summarise(diffmean = mean(diff)
                                                           , diffsd = sd(diff)
                                                           , diffmedian = median(diff)
                                                           , up = quantile(diff, prob = 0.75)
                                                           )

e = "beta2"
res %>% 
  rename(`true:VAR` = paste0("true:", e)) %>% 
  group_by(`true:VAR`) %>% 
  summarise_(`mean:VAR` = paste0("mean(", e, ")")
             , `median:VAR` = paste0("median(", e, ")")
             , `lowci:VAR` = paste0("quantile.date.or(", e, ", prob = 0.025)")
             , `highci:VAR` = paste0("quantile.date.or(", e, ", prob = 0.975)")) %>% 
  ggplot(aes(x = `true:VAR`)) +
  geom_point(aes(y = `median:VAR`, colour = "Median"), size = 1, alpha = 0.3) +
  geom_point(aes(y = `mean:VAR`, colour = "Mean"), size = 1, alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0) + 
  # geom_ribbon(aes(x = `true:VAR`, ymin = `lowci:VAR`, ymax = `highci:VAR`), colour = "grey", alpha = 0.2) +
  labs(title = "", colour = "", x = paste("true", e), y = paste("estimate", e))  + 
  coord_cartesian(ylim = upper_limits[[e]]) +
  theme_bw() + 
  theme(text = element_text(size = 20), legend.position = c(0.2, 0.8)
        , legend.background = element_rect(fill="white", linetype="solid"))


res %>% 
  ggplot(aes(x = `start:beta2`, y = beta2)) + geom_point() + 
  scale_x_continuous(trans = "log10") + 
  scale_y_continuous(trans = "log10") + 
  geom_abline(slope = 1, intercept = 0)

