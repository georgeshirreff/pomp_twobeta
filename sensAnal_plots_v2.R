library(tidyverse)
library(magrittr)
library(ggplot2)
library(gridExtra)

pars = 3
ci_interval <- 0.5*qchisq(df=pars,p=0.95)


### sensitivity analysis

# experiment_name = "posneg betaProfile Einit tinitMar5 sensAnalysis"
# experiment_name = "posneg betaProfile Einit tinitMar5 sensAnalysis E10s"
# experiment_name = "posneg betaProfile Einit tinitMar5 uncertAnalysis"
experiment_name = "posneg Inflect beta2 sensAnal"

# model = "seirRefresh_"
model = "seirInflect_"

sA <- read_csv(paste0("~/tars/output/TOY/cat/", model, experiment_name, ".csv" )) %>% 
  mutate_at(c("t_inflect", "t_init"), function(x) as.Date(x, origin = "1970-01-01")) %>% 
  # mutate(R0 = beta*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/delta)))
  mutate(R0_before = beta1*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))
         , R0_after = beta2*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3))))


sa_scen <- read_csv("~/tars/input/sensAnalysis/sensAnalysis_scenarios_v5.csv") %>% 
  mutate(t_inflect = as.numeric(t_inflect))
# sa_scen <- read_csv("~/Pasteur/tars/input/sensAnalysis/sensAnalysis_scenarios_v4.csv")

# sa_scen %>% View

sA <- sA %>% 
  left_join(sa_scen %>% select(-beta1, -beta2, -t_init) %>% 
              mutate(t_inflect = as.Date(t_inflect, origin = "1970-01-01"))) %>% 
  mutate(across(contains(c("t_in")), function(x) round(as.numeric(x - as.numeric(as.Date("2020-03-11")))))) %>% 
  mutate(across(contains(c("tinit")), function(x) round(as.numeric(x - as.numeric(as.Date("2020-03-11"))))))


# deal with some which are missing
sA$scenario_number[sA$Ze == 0.6 & sA$Zrp == 0.6] = 19
sA$scenario_name[sA$Ze == 0.6 & sA$Zrp == 0.6] = "level_Z"

now_missing = which(is.na(sA$scenario_number))
sA$scenario_number[now_missing] = sA$scenario_number[now_missing - 1]
sA$scenario_name[now_missing] = sA$scenario_name[now_missing - 1]

sA$scenario_number %>% table(useNA = "always")


baseline_values <- sa_scen %>% 
  filter(scenario_name == "baseline") %>% 
  dplyr::select(-scenario_name, -scenario_number) %>% 
  mutate(across(contains(c("t_inflect")), function(x) round(as.numeric(x - as.numeric(as.Date("2020-03-11")))))) %>% 
  
  # mutate(t_inflect = as.numeric(t_inflect)) %>% 
  t %>% 
  {tibble(var = rownames(.), baseline = as.numeric(.[, 1]))} %>% 
  mutate(var = case_when(var == "E_init" ~ "Einit"
                         , var == "t_inflect" ~ "tinflect"
                         , var == "Zis" ~ "Z"
                         , var == "kappa2" ~ "kappa23"
                         , T ~ var)) %>% 
  mutate(baseline = ifelse(var %in% c("alpha", "gamma", "delta"), signif(baseline, 2), baseline))

baseline_values %>% print(n = 1000)


var_values <-
  sa_scen %>% 
  mutate(across(contains(c("t_inflect")), function(x) round(as.numeric(x - as.numeric(as.Date("2020-03-11")))))) %>% 
  mutate(var = gsub(".*_", "", scenario_name)) %>% 
  rename(Einit = E_init, tinflect = t_inflect) %>% 
  {mutate(., var_val = apply(., 1, function(x) case_when(x["var"] == "baseline" ~ as.numeric(NA)
                                                         , x["var"] == "kappa23" ~ as.numeric(x["kappa2"])
                                                         , x["var"] == "Z" ~ as.numeric(x["Zis"])
                                                         , x["var"] == "alpha" ~ signif(as.numeric(x["alpha"]), 2)
                                                         , x["var"] == "gamma" ~ signif(as.numeric(x["gamma"]), 2)
                                                         , x["var"] == "delta" ~ signif(as.numeric(x["delta"]), 2)
                                                         # , x["var"] == "tinflect" ~ "DATE"
                                                         , T ~ as.numeric(x[as.character(x["var"])]))))} %>% 
  left_join(baseline_values) %>% 
  mutate(from = baseline
         , to = var_val) %>%
  # mutate(from = ifelse(var == "tinflect", as.Date(baseline, origin = "1970-01-01") %>% format("%d%b") %>% {gsub("^0", "", .)}, baseline)
  #        , to = ifelse(var == "tinflect", as.Date(var_val, origin = "1970-01-01") %>% format("%d%b") %>% {gsub("^0", "", .)}, var_val)) %>%
  transmute(scenario = scenario_name, var, var_val, baseline
            , scenario_label =  ifelse(scenario == "baseline", "baseline", paste0(scenario, " (", from, "->", to, ")"))
            
            # , scenario_label =  ifelse(scenario == "baseline", "baseline", paste0(scenario, " (", ifelse(var == "tinflect", as.Date(baseline, origin = "1970-01-01") %>% format("%d%b") %>% {gsub("^0", "", .)}, baseline), "->", ifelse(var == "tinflect", as.Date(var_val, origin = "1970-01-01") %>% as.character, var_val), ")"))
            ) %>% 
  mutate(scenario_label = case_when(scenario == "low_Z" ~ "low_Zx (all -> low)"
                                    , scenario == "high_Z" ~ "high_Zx (all -> high)"
                                    , scenario == "level_Z" ~ "level_Zx (all -> 0.6)"
                                    , T ~ scenario_label))
  

var_values %>% print(n = 100)


var_colours <- c(RColorBrewer::brewer.pal(11, "Paired"), RColorBrewer::brewer.pal(3, "Dark2")) %>% {c("#000000", .)}
names(var_colours) = var_values$var %>% unique


e_vec = c("beta1", "beta2", "R0before", "R0after", "t_init")

pl_list = list()

for(e in e_vec){
  

  pl_list[[e]] <- sA %>% 
    rename(scenario = scenario_name) %>% 
    left_join(var_values) %>% 
    mutate(scenario = factor(scenario, levels = var_values$scenario)
           , scenario_label = factor(scenario_label, levels = var_values$scenario_label)) %>% 
    arrange(var, scenario_number) %>% 
    group_by(scenario, var) %>% 
    mutate(ci_boundary = max(loglik) - ci_interval
           , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")) %>% 
    group_by(scenario, scenario_label, var) %>% 
    filter(ci == "in_ci") %>% 
    summarise(bestbeta1 = beta1[loglik == max(loglik)]
              , minbeta1 = min(beta1)
              , maxbeta1 = max(beta1)
              , bestbeta2 = beta2[loglik == max(loglik)]
              , minbeta2 = min(beta2)
              , maxbeta2 = max(beta2)
              , bestt_init = t_init[loglik == max(loglik)]
              , mint_init = min(t_init)
              , maxt_init = max(t_init)
              , bestR0before = R0_before[which(loglik == max(loglik))[1]]
              , minR0before = min(R0_before)
              , maxR0before = max(R0_before)
              , bestR0after = R0_after[which(loglik == max(loglik))[1]]
              , minR0after = min(R0_after)
              , maxR0after = max(R0_after)
              ) %>% 
    
    ggplot(aes_string(y = "scenario_label", x = paste0("best", e), colour = "var")) +
    geom_errorbar(aes_string(xmin=paste0("min",e), xmax=paste0("max", e)), width=.3, size = 1) +
    labs(x = e, colour = "", y = "") + #lims(x = c(0, 1)) +
    # coord_cartesian(xlim=c(NA, NA)) +
  
    # ggplot(aes(y = scenario_label, x = bestbeta1, colour = var)) +
    # geom_errorbar(aes(xmin=minbeta1, xmax=maxbeta1), width=.3, size = 1) +
    # labs(x = expression(beta[1]), colour = "", y = "") + #lims(x = c(0, 1)) +
    # coord_cartesian(xlim=c(0, NA)) +
    
    # ggplot(aes(y = scenario_label, x = bestbeta2, colour = var)) +
    # geom_errorbar(aes(xmin=minbeta2, xmax=maxbeta2), width=.3, size = 1) +
    # labs(x = expression(beta[2]), colour = "", y = "") + #lims(x = c(0, 1)) +
    # coord_cartesian(xlim=c(0, NA)) +
    
    # ggplot(aes(y = scenario_label, x = bestR0before, colour = var)) +
    # geom_errorbar(aes(xmin=minR0before, xmax=maxR0before), width=.2, size = 1) +
    # labs(x = expression(R[0]~before), colour = "", y = "") +
    # coord_cartesian(xlim=c(0, NA)) +
    # 
    # ggplot(aes(y = scenario_label, x = bestR0after, colour = var)) +
    # geom_errorbar(aes(xmin=minR0after, xmax=maxR0after), width=.2, size = 1) +
    # labs(x = expression(R[0]~after), colour = "", y = "") +
    # coord_cartesian(xlim=c(0, NA)) +
    # 
    # ggplot(aes(y = scenario_label, x = bestt_init, colour = var)) +
    # geom_errorbar(aes(xmin=mint_init, xmax=maxt_init), width=.2, size = 1) +
    # labs(x = expression(t[init]), colour = "", y = "") +
    # coord_cartesian(xlim=c(NA, NA)) +
    
    geom_point(size = 2) +
    scale_colour_manual(values = var_colours) + 
    theme_bw() + 
    guides(colour = F)+
    theme(panel.grid.major.y = element_blank()
          , axis.text.y = eval(parse(text = ifelse(e == "beta1", "element_text()", "element_blank()")))
          , text = element_text(size=10)) 
  
  # ggsave(paste0("~/Pasteur/tars/output/Figs/"
  #               , "sensAnal_", e
  #               # , "bE_beta_sensAnal"
  #               # , "bE_R0_sensAnal"
  #               # , "bE_Einit_sensAnal"
  #               , ".png")
  #        , height = 20
  #        , width = ifelse(e == "beta1", 20, 12)
  #        , units = "cm"
  # )

}

ggsave(plot = arrangeGrob(grobs = pl_list
                          , nrow = 1, widths = list(20, 12, 12, 12, 12))
       , filename = "~/tars/output/Figs/sensAnal_ALL_relative.png", units = "cm", width = 55, height = 25)


ggsave(plot = arrangeGrob(grobs = pl_list
                          , nrow = 1, widths = list(20, 12, 12, 12, 12))
       , filename = "~/tars/output/Figs/SFig8.jpeg", units = "cm", width = 30, height = 15, device = "jpeg", dpi = 300)


# ggsave(paste0("~/Pasteur/tars/output/Figs/", "bE_Einit_sensAnal", ".png")
#        , height = 20, width = 12, units = "cm"
# )






# # final results table
# 
# # all_tab
# 
# 
# final_results_table <- bind_rows(
# growth_tab %>% 
#   rename(title = phase) %>% 
#   mutate(title = case_when(title == "March + April" ~ "All data"
#                            , title == "March only" ~ "Growth phase")) %>% 
#   select(-max_loglik)
# ,
# ward_tab %>% 
#   rename(title = wardCode) %>% 
#   mutate(title = paste("Ward", title))
# ) %>% 
#   mutate_if(is.numeric, function(x) round(x, 2)) %>% 
#   transmute(title
#             , beta = paste0(bestbeta, " (", minbeta, "-", maxbeta, ")")
#             , R0 = paste0(bestR0, " (", minR0, "-", maxR0, ")")
#             ) %>% 
#   mutate_all(function(x) gsub("NA", "?", x))
# 
# final_results_table %>% write_csv(paste0("~/Pasteur/tars/output/Figs/", "final_results_table", ".csv"))
