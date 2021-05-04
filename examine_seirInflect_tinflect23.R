quantile.date.or <- function(vec, prob){
  if(class(vec) == "Date"){
    num_vec = as.numeric(vec)
    num_vec %>% quantile(prob = prob) %>% as.Date(origin = "1970-01-01")
  } else {
    vec %>% quantile(prob = prob)
  }
}
library(GGally)


library(magrittr)
library(tidyverse)

library(ggplot2)



# experiment_name = "posneg Inflect"
# experiment_name = "posneg Inflect tinitFeb20"
# experiment_name = "posneg Inflect tinitFeb27"
# experiment_name = "posneg Inflect valid1param"
# experiment_name = "posneg Inflect Mar5 Mar17"

# experiment_name = "posneg Inflect beta2 Mar5 Mar17"
# experiment_name = "posneg Inflect beta2 Mar5 Mar12"
# experiment_name = "posneg Inflect beta2 Mar5 Mar19"

# experiment_name = "posneg Inflect beta2 Feb27 Mar12"
# experiment_name = "posneg Inflect beta2 Feb27 Mar17"
# experiment_name = "posneg Inflect beta2 Feb27 Mar19"
# 
# experiment_name = "posneg Inflect beta2 Feb20 Mar12"
# experiment_name = "posneg Inflect beta2 Feb20 Mar17"
# experiment_name = "posneg Inflect beta2 Feb20 Mar19"

# for(experiment_name in c("posneg Inflect beta1profile Mar5 Mar12"
#                          , "posneg Inflect beta1profile Mar5 Mar17"
#                          , "posneg Inflect beta1profile Mar5 Mar19"
#                          , "posneg Inflect beta1profile Feb27 Mar12"
#                          , "posneg Inflect beta1profile Feb27 Mar17"
#                          , "posneg Inflect beta1profile Feb27 Mar19"
#                          , "posneg Inflect beta1profile Feb20 Mar12"
#                          , "posneg Inflect beta1profile Feb20 Mar17"
#                          , "posneg Inflect beta1profile Feb20 Mar19")){

# for(experiment_name in c("posneg Inflect beta2 Einit1"
#                          , "posneg Inflect beta2 Einit3"
#                          , "posneg Inflect beta2 Einit10"
#                          , "posnegWard Inflect beta2 Einit1")){

  
# for(experiment_name in c("posneg Inflect beta1profile Einit1"
#                          , "posneg Inflect beta1profile Einit3"
#                          , "posneg Inflect beta1profile Einit10"
#                          , "posnegWard Inflect beta1profile Einit1")){
  
# for(experiment_name in c("posneg Inflect beta1profile Einit1 highbeta1"
#                          , "posneg Inflect beta1profile Einit3 highbeta1"
#                          , "posneg Inflect beta1profile Einitactual1 highbeta1")){

# res <- read_csv(paste0("~/Pasteur/tars/output/TOY/BiasTest_TOY_", experiment_name, ".csv"))

# experiment_name = "posneg Inflect beta2 sensAnal"

# experiment_name = "posneg ALLward Inflect beta2 Einit1"
# experiment_name = "posneg ALLward Inflect beta1profile Einit1"

# experiment_name = "posneg ALLward Refresh Einit1"
# experiment_name = "posneg ALLward Refresh betaprofile Einit1"




# experiment_name = "posneg ALLward Inflect beta2 Einit1 tinflect"
# experiment_name = "posneg ALLward Inflect beta1profile Einit1 tinflect"

# experiment_name = "posneg ALLward Refresh Einit1"
# experiment_name = "posneg ALLward Refresh betaprofile Einit1"
# 
# experiment_name = "posneg ALLward Inflect beta2 Einit1" 
# experiment_name = "posneg ALLward Inflect beta1profile Einit1"
# 
# 
# experiment_name = "posneg ALLwardSide Refresh Einit1"
# experiment_name = "posneg ALLwardSide Refresh betaprofile Einit1"
# 
# experiment_name = "posneg ALLward Inflect beta2 Einit1 hypertinflect"
# experiment_name = "posneg ALLward Inflect beta2 Einit1 ABStrans"
# experiment_name = "posneg ALLward Inflect beta2 Einit1 ABStrans"
# experiment_name = "posneg ALLward Inflect beta1profile Einit1 ABStrans" 
# experiment_name = "posneg ALLward Refresh Einit1 ABStrans"
# experiment_name = "posneg ALLwardSide Refresh Einit1 ABStrans"

# #   # "posneg ALLward Refresh Einit1"
# #   # , "posneg ALLward Refresh betaprofile Einit1"
# #   # , "posneg ALLwardSide Refresh Einit1"
# #   # , "posneg ALLwardSide Refresh betaprofile Einit1"
# #   "posneg ALLward Inflect beta2 Einit1"
# #   , "posneg ALLward Inflect beta1profile Einit1"
# #   , "posneg ALLward Inflect beta2 Einit1 hypertinflect"
# #   , "posneg ALLward Inflect beta2 Einit1 ABStrans"
# # "posneg ALLward Inflect beta2 Einit1 ABStrans"
# # "posneg ALLward Inflect beta1profile Einit1 ABStrans"
# # "posneg ALLward Refresh Einit1 ABStrans"
# # , "posneg ALLwardSide Refresh Einit1 ABStrans"
# # "posneg ALLward Refresh betaprofile Einit1 topup"
#   
#   # "posneg Inflect beta2 Einit1 tinflect12"
#   # , "posneg Inflect beta2 Einit1 tinflect17"
#   # , "posneg Inflect beta2 Einit1 tinflect21"
#   # , "posneg Inflect beta2 Einit1 tinflect23"
#   # , "posneg Inflect beta1profile Einit1 tinflect12"
#   # , "posneg Inflect beta1profile Einit1 tinflect17"
#   # , "posneg Inflect beta1profile Einit1 tinflect21"
#   # , "posneg Inflect beta1profile Einit1 tinflect23"
#   
#   # "posneg Inflect beta2 Einit1 tinflect25"
#   # , "posneg Inflect beta2 Einit1 tinflect27"
#   # , "posneg Inflect beta1profile Einit1 tinflect25"
#   # , "posneg Inflect beta1profile Einit1 tinflect27"
#   
#   # "posneg Inflect beta2 Einit10 tinflect23"
#   # , "posneg Inflect beta1profile Einit10 tinflect23"
#   # , "posneg Inflect beta2 Einit3 tinflect23"
#   # , "posneg Inflect beta1profile Einit3 tinflect23"
#   
#   # "posneg Inflect beta2 Feb20 Mar23"
#   # , "posneg Inflect beta1profile Feb20 Mar23"
#   # , "posneg Inflect beta2 Feb27 Mar23"
#   # , "posneg Inflect beta1profile Feb27 Mar23"
#   # , "posneg Inflect beta2 Mar5 Mar23"
#   # , "posneg Inflect beta1profile Mar5 Mar23"
#   # 
#   # , "posneg ALLward Inflect beta2 Einit1 ABStrans tinflect23"
#   # , "posneg ALLward Inflect beta1profile Einit1 ABStrans tinflect23"
#   
#   "posneg Inflect beta1profile Einit1 tinflect23 upto5"
#   , "posneg Inflect beta1profile Einit3 tinflect23 upto5"
#   , "posneg Inflect beta1profile Einit10 tinflect23 upto5"
# for(experiment_name in c(

#   
# )){
  
  experiment_name = "posneg ALLwardSide Refresh Einit1 ABStrans"



files = list.files(path = "~/tars/output/TOY/output/", pattern = paste0("s*_", experiment_name, "_.*.csv")
                   , full.names = T)
length(files)
# files = files[grepl("[A-Z][0-3][GD]_", files)]

# files = files[grepl("C[0-3]_", files)]

# files = files[order(gsub(".*_([0-9]+)[.].*", "\\1", files) %>% as.numeric)] %>% {gsub(".*Documents", "~", .)}
# files = files[!grepl("_trace.csv", files)]

# i = 100
i = 1
for(i in 1:length(files)){
  if(i %% 100 == 0) {print(c(experiment_name, i));gc()}
  if(i == 1){
    res_read = read_csv(files[i])
    cols = spec(res_read)
  } else {
    res_read = rbind(res_read, read_csv(files[i], col_types = cols))
  }
}

res = res_read %>% unique
# res <- rbind(res, res_read)
# res$beta1 %>% table
# res$t_inflect %>% table
# res %>% select(ends_with("t_inflect")) %>% unique
filename = paste0("~/tars/output/TOY/cat/seirRefresh_", experiment_name, ".csv" )
res %>%  write_csv(filename)
# res %>%  write_csv(paste0("~/tars/output/TOY/cat/seirInflect_", experiment_name, ".csv" ))

filename %>% read_csv %>% write_delim(file = filename, delim = ";")

}

#### if necessary, convert to a semi-colon delimited file

experiment_name = "posneg Inflect beta1profile Einit1"
# convert_file = paste0("~/tars/output/TOY/cat/seirRefresh_", experiment_name, ".csv" )
convert_file = paste0("~/tars/output/TOY/cat/seirInflect_", experiment_name, ".csv" )
convert_file %>% read_csv %>% write_delim(file = convert_file, delim = ";")

####



# ##### ward-level analysis ####################
# 
# X = seq(0, 20, by = 0.1)
# plot(X, dgamma(x = X, shape = 10, scale = 0.5, log = T), type = "l")
# # plot(X, dgamma(x = X, shape = 0.1, scale = 10, log = T), type = "l")
# # plot(X, dgamma(x = X, shape = 0.1, scale = 10, log = T), type = "l")
# 
# 
# 
# 
# abline(v = 1)     
# plot(q$Beta, is.infinite(q$loglik))
# 
# exp_name = "posnegWard Inflect beta2 Einit1"
# 
# pars = 3
# ci_interval <- 0.5*qchisq(df=pars,p=0.95)
# 
# ward_noprior_prior <- list()
# 
# res_ward_prior <- rbind(read_csv(paste0("~/Pasteur/tars/output/TOY/cat/seirInflect_", exp_name, ".csv" )) %>% mutate(analysis = "beta2")
#                   , read_csv(paste0("~/Pasteur/tars/output/TOY/cat/seirInflect_", gsub("beta2", "beta1profile", exp_name), ".csv" )) %>% mutate(analysis = "beta1profile"))%>% 
#   mutate(R0_before = beta1*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>% 
#   mutate(R0_after = beta2*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>% 
#   mutate_at(c("t_inflect", "t_init", "start_tinit"), function(x) as.Date(x, origin = "1970-01-01")) %>% 
#   rename(loglik_data = loglik) %>% 
#   # mutate(loglik_prior = dunif(x = beta1, min = 0, max = 5, log = T)) %>% 
#   # mutate(loglik_prior = dgamma(x = beta1, shape = 10, scale = 0.5, log = T)) %>% 
#   mutate(loglik_prior = dgamma(x = beta1, shape = 0.1, scale = 10, log = T)) %>%
#   mutate(loglik = loglik_data + loglik_prior) %>% 
#   group_by(wardCode) %>% 
#   mutate(ci_boundary = max(loglik) - ci_interval
#          , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")) %>% 
#   ungroup
# 
# res_ward_noprior <- rbind(read_csv(paste0("~/Pasteur/tars/output/TOY/cat/seirInflect_", exp_name, ".csv" )) %>% mutate(analysis = "beta2")
#                   , read_csv(paste0("~/Pasteur/tars/output/TOY/cat/seirInflect_", gsub("beta2", "beta1profile", exp_name), ".csv" )) %>% mutate(analysis = "beta1profile"))%>% 
#   mutate(R0_before = beta1*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>% 
#   mutate(R0_after = beta2*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>% 
#   mutate_at(c("t_inflect", "t_init", "start_tinit"), function(x) as.Date(x, origin = "1970-01-01")) %>% 
#   group_by(wardCode) %>% 
#   mutate(ci_boundary = max(loglik) - ci_interval
#          , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")) %>% 
#   ungroup
# 
# ward_noprior_prior[["beta1_noprior"]] <- res_ward_noprior %>%
#   group_by(wardCode) %>% 
#   ggplot(aes(x = beta1, y = loglik, alpha = ci, colour = wardCode)) + geom_point() + 
#   geom_hline(aes(yintercept = ci_boundary, colour = wardCode), linetype = "dashed") + 
#   # facet_grid(~Prior, scales = "free_y") +
#   scale_alpha_manual(values = c(in_ci = 0.5, out_ci = 0.01)) + 
#   # scale_x_continuous(breaks = seq(0, 2, by = 0.5), minor_breaks = seq(0, 10, by = 0.1)) + 
#   labs(x = expression(beta[1]), y = "log Likelihood", alpha = "") +
#   theme_bw() +
#   theme(text = element_text(size = 20)
#         , panel.grid.major.x = element_line(size = 1)) + 
#   guides(alpha = F) + 
#   coord_cartesian(ylim = c(-80, NA)
#                   , xlim = c(0, 30)
#   )
# 
# ward_noprior_prior[["beta2_noprior"]] <- res_ward_noprior %>%
#   group_by(wardCode) %>% 
#   ggplot(aes(x = beta2, y = loglik, alpha = ci, colour = wardCode)) + geom_point() + 
#   geom_hline(aes(yintercept = ci_boundary, colour = wardCode), linetype = "dashed") + 
#   # facet_grid(~Prior, scales = "free_y") +
#   scale_alpha_manual(values = c(in_ci = 0.5, out_ci = 0.01)) + 
#   # scale_x_continuous(breaks = seq(0, 2, by = 0.5), minor_breaks = seq(0, 10, by = 0.1)) + 
#   labs(x = expression(beta[2]), y = "log Likelihood", alpha = "") +
#   theme_bw() +
#   theme(text = element_text(size = 20)
#         , panel.grid.major.x = element_line(size = 1)) + 
#   guides(alpha = F) + 
#   coord_cartesian(ylim = c(-80, NA)
#                   , xlim = c(0, 1)
#   ) #+ scale_x_log10()
# 
# ward_noprior_prior[["t_init_noprior"]] <- res_ward_noprior %>%
#   group_by(wardCode) %>% 
#   ggplot(aes(x = t_init, y = loglik, alpha = ci, colour = wardCode)) + geom_point() + 
#   geom_hline(aes(yintercept = ci_boundary, colour = wardCode), linetype = "dashed") + 
#   # facet_grid(~Prior, scales = "free_y") +
#   scale_alpha_manual(values = c(in_ci = 0.5, out_ci = 0.01)) + 
#   # scale_x_continuous(breaks = seq(0, 2, by = 0.5), minor_breaks = seq(0, 10, by = 0.1)) + 
#   labs(x = expression(t[init]), y = "log Likelihood", alpha = "") +
#   theme_bw() +
#   theme(text = element_text(size = 20)
#         , axis.text.x = element_text(angle = 90)
#         , panel.grid.major.x = element_line(size = 1)) + 
#   guides(alpha = F) + 
#   coord_cartesian(ylim = c(-80, NA)
#                   # , xlim = c(0, 2.5)
#   ) #+ scale_x_log10()
# 
# 
# 
# ward_noprior_prior[["beta1_prior"]] <- res_ward_prior %>%
#   group_by(wardCode) %>% 
#   ggplot(aes(x = beta1, y = loglik, alpha = ci, colour = wardCode)) + geom_point() + 
#   geom_hline(aes(yintercept = ci_boundary, colour = wardCode), linetype = "dashed") + 
#   # facet_grid(~Prior, scales = "free_y") +
#   scale_alpha_manual(values = c(in_ci = 0.5, out_ci = 0.01)) + 
#   # scale_x_continuous(breaks = seq(0, 2, by = 0.5), minor_breaks = seq(0, 10, by = 0.1)) + 
#   # labs(x = expression(beta[1]), y = "log Likelihood", alpha = "") + 
#   labs(x = expression(beta[1]), y = "log Likelihood with prior", alpha = "") + 
#   theme_bw() +
#   theme(text = element_text(size = 20)
#         , panel.grid.major.x = element_line(size = 1)) + 
#   guides(alpha = F) + 
#   coord_cartesian(ylim = c(-80, NA)
#                   , xlim = c(0, 30)
#   )
# 
# ward_noprior_prior[["beta2_prior"]] <- res_ward_prior %>%
#   group_by(wardCode) %>% 
#   ggplot(aes(x = beta2, y = loglik, alpha = ci, colour = wardCode)) + geom_point() + 
#   geom_hline(aes(yintercept = ci_boundary, colour = wardCode), linetype = "dashed") + 
#   # facet_grid(~Prior, scales = "free_y") +
#   scale_alpha_manual(values = c(in_ci = 0.5, out_ci = 0.01)) + 
#   # scale_x_continuous(breaks = seq(0, 2, by = 0.5), minor_breaks = seq(0, 10, by = 0.1)) + 
#   labs(x = expression(beta[2]), y = "log Likelihood with prior", alpha = "") + 
#   theme_bw() +
#   theme(text = element_text(size = 20)
#         , panel.grid.major.x = element_line(size = 1)) + 
#   guides(alpha = F) + 
#   coord_cartesian(ylim = c(-80, NA)
#                   , xlim = c(0, 1)
#   ) #+ scale_x_log10()
# 
# ward_noprior_prior[["t_init_prior"]] <- res_ward_prior %>%
#   
#   group_by(wardCode) %>% 
#   ggplot(aes(x = t_init, y = loglik, alpha = ci, colour = wardCode)) + geom_point() + 
#   geom_hline(aes(yintercept = ci_boundary, colour = wardCode), linetype = "dashed") + 
#   # facet_grid(~Prior, scales = "free_y") +
#   scale_alpha_manual(values = c(in_ci = 0.5, out_ci = 0.01)) + 
#   # scale_x_continuous(breaks = seq(0, 2, by = 0.5), minor_breaks = seq(0, 10, by = 0.1)) + 
#   labs(x = expression(t[init]), y = "log Likelihood with prior", alpha = "") + 
#   theme_bw() +
#   theme(text = element_text(size = 20)
#         , axis.text.x = element_text(angle = 90)
#         , panel.grid.major.x = element_line(size = 1)) + 
#   guides(alpha = F) + 
#   coord_cartesian(ylim = c(-80, NA)
#                   # , xlim = c(0, 2.5)
#   ) #+ scale_x_log10()
# 
# # this_legend <- cowplot::get_legend(ward_noprior_prior[["t_init_prior"]])
# # ward_noprior_prior[["t_init_prior"]] <- ward_noprior_prior[["t_init_prior"]] + theme(legend.position = "none") 
# 
# # ward_noprior_prior[["legend"]] <- this_legend
# 
# ggsave(ggpubr::ggarrange(plotlist = ward_noprior_prior, ncol = 3, nrow = 2, common.legend = T, legend = "right")
#        , filename = paste0("~/Pasteur/tars/output/Figs/", "inflect_ward_beta", ".png")
#        , width = 25, height = 18, units = "cm")
# 
# 
# 
# 
# wtable <- res_ward_prior %>%  
#   group_by(wardCode, E_init) %>% 
#   mutate(ci_boundary = max(loglik) - ci_interval
#          , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")
#          , AIC = 2*pars - 2*max(loglik)) %>% 
#   filter(ci == "in_ci") %>%
#   summarise(max_loglik = max(loglik)
#             , bestbeta1 = beta1[loglik == max(loglik)]
#             , minbeta1 = min(beta1)
#             , maxbeta1 = max(beta1)
#             , bestbeta2 = beta2[loglik == max(loglik)]
#             , minbeta2 = min(beta2)
#             , maxbeta2 = max(beta2)
#             , bestt_init = t_init[loglik == max(loglik)]
#             , mint_init = min(t_init)
#             , maxt_init = max(t_init)
#             , bestR0before = R0_before[which(loglik == max(loglik))[1]]
#             , minR0before = min(R0_before)
#             , maxR0before = max(R0_before)
#             , bestR0after = R0_after[which(loglik == max(loglik))[1]]
#             , minR0after = min(R0_after)
#             , maxR0after = max(R0_after)
#             , AIC = mean(AIC)
#   ) %>% 
#   ungroup %>% 
#   mutate(across(contains("t_in"), function(x) gsub("^0", " ", format(x, "%d %b")))) %>% 
#   mutate_if(is.numeric, function(x) format(round(x, 2), nsmall = 2)) %>%
#   # mutate_at(c("bestR0", "minR0", "maxR0", "bestEinit", "minEinit", "maxEinit"), function(x) format(round(x, 1), nsmall = 1)) %>%
#   transmute(wardCode
#             , E_init
#             , beta1 = paste0(bestbeta1, "\n(", minbeta1, "-", maxbeta1, ")")
#             # , beta_factor = paste0(bestbeta_factor, " (", minbeta_factor, "-", maxbeta_factor, ")")
#             , beta2 = paste0(bestbeta2, "\n(", minbeta2, "-", maxbeta2, ")")
#             
#             , R0_before = paste0(bestR0before, "\n(", minR0before, "-", maxR0before, ")")
#             , R0_after = paste0(bestR0after, "\n(", minR0after, "-", maxR0after, ")")
#             
#             , t_init = paste0(bestt_init, "\n(", mint_init, "-", maxt_init, ")")
#             , AIC
#             
#   ) %>% 
#   mutate(E_init = as.numeric(E_init %>% trimws))
# 
# wtable %>% write_csv(paste0("~/Pasteur/tars/output/Figs/", "inflect_ward_results_table_tinit", ".csv"))


################ fixed Einit ##########

res = NULL
for(Eini in c(1, 3, 10)){
  for(analysis in c("beta2", "beta1profile")){
    # exp_name = paste0("posneg Inflect ", analysis, " Einit", Eini)
    exp_name = paste0("posneg Inflect ", analysis, " Einit", Eini, " tinflect23")
    print(exp_name)
    res_piece = read_delim(paste0("~/tars/output/TOY/cat/seirInflect_", exp_name, ".csv" ), delim = ";") %>% mutate(analysis = analysis)
    # res_piece = read_csv(paste0("~/tars/output/TOY/cat/seirInflect_", exp_name, ".csv" )) %>% mutate(analysis = analysis)
    if(is.null(res)){
      res = res_piece
    } else {
      res = rbind(res, res_piece)
    }
    
  }
}

final_date = as.Date("2020-04-30")

pars = 3
ci_interval <- 0.5*qchisq(df=pars,p=0.95)

res <- res %>% 
  mutate(R0_before = beta1*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>% 
  mutate(R0_after = beta2*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>% 
  mutate_at(c("t_inflect", "t_init", "start_tinit"), function(x) as.Date(x, origin = "1970-01-01")) %>% 
  mutate(time_phase1 = max(0, t_inflect - t_init)
         , time_phase2 = max(0, final_date - t_inflect)
         , R0 = (R0_before*time_phase1 + R0_after*time_phase2)/(time_phase1 + time_phase2)) %>%
  mutate(risk_ratio = beta2/beta1) %>% 
  group_by(E_init) %>% 
  mutate(ci_boundary = max(loglik) - ci_interval
         , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")) %>% 
  ungroup


  



itable <- res %>%  
  group_by(E_init) %>% 
  mutate(ci_boundary = max(loglik) - ci_interval
         , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")
         , AIC = 2*pars - 2*max(loglik)) %>% 
  filter(ci == "in_ci") %>%
  summarise(max_loglik = max(loglik)
            , bestbeta1 = beta1[loglik == max(loglik)]
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
            , bestR0 = R0[which(loglik == max(loglik))[1]]
            , minR0 = min(R0)
            , maxR0 = max(R0)
            , bestrisk_ratio = risk_ratio[which(loglik == max(loglik))[1]]
            , minrisk_ratio = min(risk_ratio)
            , maxrisk_ratio = max(risk_ratio)
            
            , AIC = mean(AIC)
  ) %>% 
  ungroup %>% 
  mutate(across(contains("t_in"), function(x) gsub("^0", " ", format(x, "%d %b")))) %>% 
  mutate_if(is.numeric, function(x) format(round(x, 2), nsmall = 2)) %>%
  # mutate_at(c("bestR0", "minR0", "maxR0", "bestEinit", "minEinit", "maxEinit"), function(x) format(round(x, 1), nsmall = 1)) %>%
  transmute(E_init
            , beta1 = paste0(bestbeta1, "\n(", minbeta1, "-", maxbeta1, ")")
            # , beta_factor = paste0(bestbeta_factor, " (", minbeta_factor, "-", maxbeta_factor, ")")
            , beta2 = paste0(bestbeta2, "\n(", minbeta2, "-", maxbeta2, ")")
            
            , R0_before = paste0(bestR0before, "\n(", minR0before, "-", maxR0before, ")")
            , R0_after = paste0(bestR0after, "\n(", minR0after, "-", maxR0after, ")")
            
            , R0 = paste0(bestR0, "\n(", minR0, "-", maxR0, ")")
            , risk_ratio = paste0(bestrisk_ratio, "\n(", minrisk_ratio, "-", maxrisk_ratio, ")")
            
            , t_init = paste0(bestt_init, "\n(", mint_init, "-", maxt_init, ")")
            , AIC
            
  ) %>% 
  mutate(E_init = as.numeric(E_init %>% trimws))

itable %>% write_delim(paste0("~/tars/output/Figs/", "inflect_results_table_tinit", ".csv"), delim = ";")


inflect_results_scatter_tinit = list()

inflect_results_scatter_tinit[["beta1"]] <- res %>% 
  ggplot(aes(x = beta1, y = loglik, alpha = ci)) + geom_point() + 
  geom_hline(aes(yintercept = ci_boundary), linetype = "dashed") + 
  facet_grid(~E_init, scales = "free_y") +
  scale_alpha_manual(values = c(in_ci = 0.5, out_ci = 0.01)) + 
  coord_cartesian(ylim = c(-330, NA)
                  , xlim = c(0, 5)
  ) + 
  labs(x = expression(beta[1]), y = "log Likelihood") + 
  theme_bw() +
  theme(text = element_text(size = 20)
        , panel.grid.major.x = element_line(size = 1)) + 
  guides(alpha = F) 


inflect_results_scatter_tinit[["beta2"]] <- res %>% 
  ggplot(aes(x = beta2, y = loglik, alpha = ci)) + geom_point() + 
  geom_hline(aes(yintercept = ci_boundary), linetype = "dashed") + 
  facet_grid(~E_init, scales = "free_y") +
  scale_alpha_manual(values = c(in_ci = 0.5, out_ci = 0.01)) + 
  coord_cartesian(ylim = c(-330, NA)
                  , xlim = c(0, 0.6)
  ) + 
  labs(x = expression(beta[2]), y = "log Likelihood") + 
  theme_bw() +
  theme(text = element_text(size = 20)
        , panel.grid.major.x = element_line(size = 1)) + 
  guides(alpha = F) 

inflect_results_scatter_tinit[["t_init"]] <- res %>% 
  ggplot(aes(x = t_init, y = loglik, alpha = ci)) + geom_point() + 
  geom_hline(aes(yintercept = ci_boundary), linetype = "dashed") + 
  facet_grid(~E_init, scales = "free_y") +
  scale_alpha_manual(values = c(in_ci = 0.5, out_ci = 0.01)) + 
  coord_cartesian(ylim = c(-330, NA)
                  # , xlim = c(0, 2.5)
  ) + 
  labs(x = expression(t[init]), y = "log Likelihood") + 
  theme_bw() +
  theme(text = element_text(size = 20)
        , axis.text.x = element_text(angle = 90)
        , panel.grid.major.x = element_line(size = 1)) + 
  guides(alpha = F) 





ggsave(ggpubr::ggarrange(plotlist = inflect_results_scatter_tinit, ncol = 1)
       , filename = paste0("~/tars/output/Figs/", "inflect_results_scatter_tinit", ".png")
       , width = 20, height = 25, units = "cm")



################ test tinflect ########

res = NULL
exp_name = "posneg Inflect beta2 Einit1 tinflect12"
for(exp_name in c("posneg Inflect beta2 Einit1 tinflect12"
                  , "posneg Inflect beta2 Einit1 tinflect17"
                  , "posneg Inflect beta2 Einit1"
                  , "posneg Inflect beta2 Einit1 tinflect21"
                  , "posneg Inflect beta2 Einit1 tinflect23"
                  , "posneg Inflect beta2 Einit1 tinflect25"
                  , "posneg Inflect beta2 Einit1 tinflect27"
                  , "posneg Inflect beta1profile Einit1 tinflect12"
                  , "posneg Inflect beta1profile Einit1 tinflect17"
                  , "posneg Inflect beta1profile Einit1"
                  , "posneg Inflect beta1profile Einit1 tinflect21"
                  , "posneg Inflect beta1profile Einit1 tinflect23"
                  , "posneg Inflect beta1profile Einit1 tinflect25"
                  , "posneg Inflect beta1profile Einit1 tinflect27")){
  res_piece = read_delim(paste0("~/tars/output/TOY/cat/seirInflect_", exp_name, ".csv" ), delim = ";")
  
    
  if(is.null(res)){
    res = res_piece
  } else {
    res = rbind(res, res_piece)
  }
  
}



final_date = as.Date("2020-04-30")

pars = 3
ci_interval <- 0.5*qchisq(df=pars,p=0.95)

res <- res %>% 
  mutate(R0_before = beta1*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>% 
  mutate(R0_after = beta2*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>% 
  mutate_at(c("t_inflect", "t_init", "start_tinit"), function(x) as.Date(x, origin = "1970-01-01")) %>% 
  mutate(time_phase1 = max(0, t_inflect - t_init)
         , time_phase2 = max(0, final_date - t_inflect)
         , R0 = (R0_before*time_phase1 + R0_after*time_phase2)/(time_phase1 + time_phase2)) %>%
  mutate(risk_ratio = beta2/beta1) %>% 
  group_by(E_init) %>% 
  mutate(ci_boundary = max(loglik) - ci_interval
         , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")) %>% 
  ungroup


tinf_table <- res %>%  
  group_by(t_inflect) %>% 
  mutate(ci_boundary = max(loglik) - ci_interval
         , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")
         , AIC = 2*pars - 2*max(loglik)) %>% 
  filter(ci == "in_ci") %>%
  summarise(max_loglik = max(loglik)
            , bestbeta1 = beta1[loglik == max(loglik)]
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
            , bestR0 = R0[which(loglik == max(loglik))[1]]
            , minR0 = min(R0)
            , maxR0 = max(R0)
            , bestrisk_ratio = risk_ratio[which(loglik == max(loglik))[1]]
            , minrisk_ratio = min(risk_ratio)
            , maxrisk_ratio = max(risk_ratio)
            
            , best_eff = 1-risk_ratio[which(loglik == max(loglik))[1]]
            , min_eff = 1-max(risk_ratio)
            , max_eff = 1-min(risk_ratio)
            
            , AIC = mean(AIC)
  ) %>% 
  ungroup %>% 
  mutate(across(contains("t_in"), function(x) gsub("^0", " ", format(x, "%d %b")))) %>% 
  mutate_if(is.numeric, function(x) format(round(x, 2), nsmall = 2)) %>%
  # mutate_at(c("bestR0", "minR0", "maxR0", "bestEinit", "minEinit", "maxEinit"), function(x) format(round(x, 1), nsmall = 1)) %>%
  transmute(t_inflect
            , beta1 = paste0(bestbeta1, "\n(", minbeta1, "-", maxbeta1, ")")
            # , beta_factor = paste0(bestbeta_factor, " (", minbeta_factor, "-", maxbeta_factor, ")")
            , beta2 = paste0(bestbeta2, "\n(", minbeta2, "-", maxbeta2, ")")
            
            , R0_before = paste0(bestR0before, "\n(", minR0before, "-", maxR0before, ")")
            , R0_after = paste0(bestR0after, "\n(", minR0after, "-", maxR0after, ")")
            
            , R0 = paste0(bestR0, "\n(", minR0, "-", maxR0, ")")
            , risk_ratio = paste0(bestrisk_ratio, "\n(", minrisk_ratio, "-", maxrisk_ratio, ")")
            , eff = paste0(best_eff, "\n(", min_eff, "-", max_eff, ")")
            
            , t_init = paste0(bestt_init, "\n(", mint_init, "-", maxt_init, ")")
            , AIC
            
  )

tinf_table %>% write_delim(paste0("~/tars/output/Figs/", "inflect_results_table_tinflect", ".csv"), delim = ";")

##### t_inflect sim plots #####

res = NULL
exp_name = "posneg Inflect beta2 Einit1 tinflect12"
for(exp_name in c("posneg Inflect beta2 Einit1 tinflect12"
                  , "posneg Inflect beta2 Einit1 tinflect17"
                  , "posneg Inflect beta2 Einit1"
                  , "posneg Inflect beta2 Einit1 tinflect21"
                  , "posneg Inflect beta2 Einit1 tinflect23"
                  , "posneg Inflect beta2 Einit1 tinflect25"
                  , "posneg Inflect beta2 Einit1 tinflect27"
                  , "posneg Inflect beta1profile Einit1 tinflect12"
                  , "posneg Inflect beta1profile Einit1 tinflect17"
                  , "posneg Inflect beta1profile Einit1"
                  , "posneg Inflect beta1profile Einit1 tinflect21"
                  , "posneg Inflect beta1profile Einit1 tinflect23"
                  , "posneg Inflect beta1profile Einit1 tinflect25"
                  , "posneg Inflect beta1profile Einit1 tinflect27")){
  res_piece = read_delim(paste0("~/tars/output/TOY/cat/seirInflect_", exp_name, ".csv" ), delim = ";")
  
  # DO SIMS HERE!!!
  
  if(is.null(res)){
    res = res_piece
  } else {
    res = rbind(res, res_piece)
  }
  
}

pars = 3
ci_interval <- 0.5*qchisq(df=pars,p=0.95)


source('~/pomp_twobeta/sim_plot_wards.R')
source('~/pomp_twobeta/seirInflect_source.R')
t_inflect_vec = res$t_inflect %>% unique

# t_i = t_inflect_vec[1]
for(t_i in t_inflect_vec){
  print(as.Date(t_i, origin = "1970-01-01"))
  tres_piece = pomp_sim_distribution(seirInflect
                                     , res %>%  
                                       group_by(t_inflect) %>% 
                                       mutate(ci_boundary = max(loglik) - ci_interval
                                              , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")) %>% 
                                        filter(t_inflect == t_i) %>% 
                                        filter(ci == "in_ci") %>% 
                                       select(names(seirInflect@params))
                                     , NSIM = 1000, SAR_numer_threshold = 3
                                     , calculate_logLik = T) %>% 
    mutate(t_inflect = t_i)
  
  if(t_i == t_inflect_vec[1]){
    tres = tres_piece
  } else {
    tres = rbind(tres, tres_piece)
  }
  
  
}



sim_plot = tres %>% 
  mutate_at(c("t_inflect", "Date"), function(x) as.Date(x, origin = "1970-01-01")) %>% 
  filter(Date > as.Date("2020-03-01")) %>% 
  ggplot(aes(x = Date)) + 
  # geom_bar(aes(y = Data, fill = "Data"), stat = "identity") + 
  geom_ribbon(aes(ymin = lowCI, ymax = highCI), alpha = 0.6, fill = "grey") +
  geom_line(aes(y = highCI, colour = "CI", size = "CI", linetype = "CI")) +
  geom_line(aes(y = lowCI, colour = "CI", size = "CI", linetype = "CI")) +
  # geom_line(aes(y = max_logLik, colour = "bestFit", size = "bestFit", linetype = "bestFit")) +
  geom_line(aes(y = median, colour = "median", size = "median", linetype = "median")) +
  geom_point(aes(y = Data, colour = "Data", size = "Data", linetype = "Data")) + 
  scale_colour_manual(values = c(  Data = "red"   , bestFit = "blue", median = "black" , mode = "blue", CI = "grey")) +
  scale_linetype_manual(values = c(Data = "blank", bestFit = "dashed", median = "dashed", mode = "dotted", CI = "solid")) +
  scale_size_manual(values = c(    Data = 2       , bestFit = 1, median = 1, mode = 1       , CI = 1)) +
  geom_vline(aes(xintercept = t_inflect), linetype = "dotted") + 
  labs(x = "", y = "Positive tests", linetype = "Simulations", size = "Simulations", colour = "Simulations") + 
  theme_bw() + theme(text = element_text(size = 20)
                     # , legend.position = c(0.8, 0.2)
                     , legend.background = element_rect(fill="white",
                                                        size=0.5, linetype="solid")
                     , axis.text.x = element_text(angle = 90)
                     
  ) + 
  facet_wrap(.~t_inflect)

ggsave(sim_plot, filename = "~/tars/output/Figs/inflect_results_sims_tinflect_best.png", width = 45, heigh = 35, units = "cm")

# ggsave(sim_plot, filename = "~/tars/output/Figs/inflect_results_sims_tinflect.png", width = 45, heigh = 35, units = "cm")


tres %>% 
  mutate_at(c("t_inflect", "Date"), function(x) as.Date(x, origin = "1970-01-01")) %>% 
  filter(Date > as.Date("2020-03-01")) %>% 
  mutate(diff = Data - median) %>% 
  # group_by(t_inflect) %>% summarise(sum(diff))
  ggplot(aes(x = Date, y = diff)) + 
  geom_bar(stat = "identity")+ 
  facet_wrap(.~t_inflect) + 
  labs(y = "Distance from data to simulation median")

################ fixed tinit ##########

res = NULL
for(tini in c("Feb20", "Feb27", "Mar5")){
  # for(tinf in c("Mar12", "Mar17", "Mar19")){
  for(tinf in c("Mar23")){
    for(analysis in c("beta2", "beta1profile")){
      exp_name = paste("posneg Inflect", analysis, tini, tinf)
      
      res_piece = read_delim(paste0("~/tars/output/TOY/cat/seirInflect_", exp_name, ".csv" ), delim = ";") %>% 
        mutate(analysis = analysis)
     
      if(is.null(res)){
        res = res_piece
      } else {
        res = rbind(res, res_piece)
      }
    }
  }
}


pars = 3
ci_interval <- 0.5*qchisq(df=pars,p=0.95)

res <- res %>% 
  mutate(R0_before = beta1*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>% 
  mutate(R0_after = beta2*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>% 
  mutate_at(c("t_inflect", "t_init", "start_tinit"), function(x) as.Date(x, origin = "1970-01-01")) %>% 
  group_by(t_init, t_inflect) %>% 
  mutate(ci_boundary = max(loglik) - ci_interval
         , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")) %>% 
  ungroup
  

itable <- res %>%  
  group_by(t_init, t_inflect) %>% 
  mutate(ci_boundary = max(loglik) - ci_interval
         , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")
         , AIC = 2*pars - 2*max(loglik)) %>% 
  filter(ci == "in_ci") %>%
  summarise(max_loglik = max(loglik)
            , bestbeta1 = beta1[loglik == max(loglik)]
            , minbeta1 = min(beta1)
            , maxbeta1 = max(beta1)
            , bestbeta2 = beta2[loglik == max(loglik)]
            , minbeta2 = min(beta2)
            , maxbeta2 = max(beta2)
            , bestEinit = E_init[loglik == max(loglik)]
            , minEinit = min(E_init)
            , maxEinit = max(E_init)
            , bestR0before = R0_before[which(loglik == max(loglik))[1]]
            , minR0before = min(R0_before)
            , maxR0before = max(R0_before)
            , bestR0after = R0_after[which(loglik == max(loglik))[1]]
            , minR0after = min(R0_after)
            , maxR0after = max(R0_after)
            , AIC = mean(AIC)
  ) %>% 
  ungroup %>% 
  mutate(across(contains("t_in"), function(x) gsub("^0", " ", format(x, "%d %b")))) %>% 
  mutate_if(is.numeric, function(x) format(round(x, 2), nsmall = 2)) %>%
  # mutate_at(c("bestR0", "minR0", "maxR0", "bestEinit", "minEinit", "maxEinit"), function(x) format(round(x, 1), nsmall = 1)) %>%
  transmute(t_init
            , t_inflect
            , beta1 = paste0(bestbeta1, "\n(", minbeta1, "-", maxbeta1, ")")
            # , beta_factor = paste0(bestbeta_factor, " (", minbeta_factor, "-", maxbeta_factor, ")")
            , beta2 = paste0(bestbeta2, "\n(", minbeta2, "-", maxbeta2, ")")
            
            , R0_before = paste0(bestR0before, "\n(", minR0before, "-", maxR0before, ")")
            , R0_after = paste0(bestR0after, "\n(", minR0after, "-", maxR0after, ")")
            
            , E_init = ifelse(minEinit == maxEinit, minEinit, paste0(bestEinit, "\n(", minEinit, "-", maxEinit, ")"))
            , AIC
            
  ) %>% 
  mutate(E_init = gsub("^([ ]?[0-9]+)[.]0$", "\\1", E_init)) %>% 
  mutate(E_init = gsub("\\( ", "\\(", E_init))

itable %>% write_delim(paste0("~/tars/output/Figs/", "inflect_results_table_Einit", ".csv"), delim = ";")






p_beta1 <- res %>% 
  # filter(t_inflect == as.Date("2020-03-19")) %>% 
  # mutate(t_init_expr = expression(paste0(t[init], get(t_init))))
  ggplot(aes(x = beta1, y = loglik, alpha = ci)) + geom_point() + 
  scale_alpha_manual(values = c(in_ci = 0.5, out_ci = 0.01)) + 
  coord_cartesian(ylim = c(-335, -310)
                  , xlim = c(0, 5)) + 
  facet_grid(. ~ t_init) + 
  geom_hline(aes(yintercept = ci_boundary), linetype = "dashed") + 
  theme_bw() + 
  labs(x = expression(beta[1]), y = "log Likelihood") + 
  theme(text = element_text(size = 20)
        , panel.grid.major.x = element_line(size = 1)) + 
  guides(alpha = F)


# ggsave(p_beta1, filename = paste0("~/Pasteur/tars/output/Figs/seirInflect_posneg Inflect beta1beta2Einit beta1.png")
#        , height = 30, width = 40, units = "cm"
# )

gc()

p_beta2 <- res %>% 
  # mutate(t_init_expr = expression(paste0(t[init], get(t_init))))
  # filter(t_inflect == as.Date("2020-03-19")) %>% 
  ggplot(aes(x = beta2, y = loglik, alpha = ci)) + geom_point() + 
  scale_alpha_manual(values = c(in_ci = 0.5, out_ci = 0.01)) + 
  coord_cartesian(ylim = c(-335, -310), xlim = c(0, 1.1)) + 
  facet_grid(. ~ t_init) + 
  geom_hline(aes(yintercept = ci_boundary), linetype = "dashed") + 
  theme_bw() + 
  labs(x = expression(beta[2]), y = "log Likelihood") + 
  theme(text = element_text(size = 20)
        , panel.grid.major.x = element_line(size = 1)) + 
  guides(alpha = F)

# ggsave(p_beta2, filename = paste0("~/Pasteur/tars/output/Figs/seirInflect_posneg Inflect beta1beta2Einit beta2.png")
#        , height = 30, width = 40, units = "cm"
# )

gc()


p_Einit <- res %>% 
  # mutate(t_init_expr = expression(paste0(t[init], get(t_init))))
  # filter(t_inflect == as.Date("2020-03-19")) %>% 
  ggplot(aes(x = E_init, y = loglik, alpha = ci)) + geom_point() + 
  scale_alpha_manual(values = c(in_ci = 0.5, out_ci = 0.01)) + 
  coord_cartesian(ylim = c(-335, -310), xlim = c(0.1, 100)) + 
  facet_grid(. ~ t_init) + 
  geom_hline(aes(yintercept = ci_boundary), linetype = "dashed") + 
  scale_x_log10() + 
  theme_bw() + 
  labs(x = expression(E[init]), y = "log Likelihood") + 
  theme(text = element_text(size = 20)
        # , axis.text.x = element_text(colour = )
        , panel.grid.major.x = element_line(size = 1)) + 
  guides(alpha = F)

# ggsave(p_Einit, filename = paste0("~/Pasteur/tars/output/Figs/seirInflect_posneg Inflect beta1beta2Einit Einit.png")
#        , height = 30, width = 40, units = "cm"
# )


ggsave(ggpubr::ggarrange(plotlist = list(p_beta1, p_beta2, p_Einit), ncol = 1)
       , filename = paste0("~/tars/output/Figs/", "inflect_results_scatter_Einit", ".png")
       , width = 20, height = 25, units = "cm")




# p + coord_cartesian(ylim = c(NA, -315))

############

# experiment_name = "posneg Inflect"
# experiment_name = "posneg Inflect tinitFeb27"
# experiment_name = "posneg Inflect tinitFeb20"
# experiment_name = "posneg Inflect Mar5 Mar17"

# experiment_name = "posneg Inflect beta2 Mar5 Mar12"
# experiment_name = "posneg Inflect beta2 Mar5 Mar17"
# experiment_name = "posneg Inflect beta2 Mar5 Mar19"

# experiment_name = "posneg Inflect beta2 Feb27 Mar12"
# experiment_name = "posneg Inflect beta2 Feb27 Mar17"
# experiment_name = "posneg Inflect beta2 Feb27 Mar19"
# 
# experiment_name = "posneg Inflect beta2 Feb20 Mar12"
# experiment_name = "posneg Inflect beta2 Feb20 Mar17"
experiment_name = "posneg Inflect beta2 Feb20 Mar19"

pars = 3
ci_interval <- 0.5*qchisq(df=pars,p=0.95)

res <-  read_csv(paste0("~/Pasteur/tars/output/TOY/cat/seirInflect_", experiment_name, ".csv" )) %>% 
  mutate_at(c("t_inflect", "t_init", "start_tinit"), function(x) as.Date(x, origin = "1970-01-01")) %>% 
  mutate(R0_before = beta1*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>% 
  mutate(R0_after = beta2*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>% 
  mutate(ci_boundary = max(loglik) - ci_interval
         , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci"))


res %>% group_by_all %>% summarise(n = n()) %>% pull(n) %>% table
res %>% 
  group_by(beta1, beta2, seed, start_tinit, start_tinflect, rep, loglik) %>% summarise(n = n()) %>% pull(n) %>% table

res %>% 
  ggplot(aes(x = beta1, y = loglik, colour = ci)) + 
  coord_cartesian(ylim = c(max(res$loglik)-2*ci_interval, NA)) + 
  geom_point()



res %>% 
  # filter(ci == "in_ci") %>% 
  ggpairs(columns = c("loglik", "beta1", "beta2", "E_init")
          , upper = list(continuous = "points")
          , lower = list(continuous = "points"), aes(colour = ci))


ggsave(paste0("~/Pasteur/tars/output/Inflection/", "matrix_plot", experiment_name, ".png"), height = 20, width = 30, units = "cm")


inflect_tab <- res %>% 
  filter(ci == "in_ci") %>% 
  ungroup %>% 
  summarise(bestbeta1 = beta1[which(loglik == max(loglik))[1]]
            , minbeta1 = min(beta1)
            , maxbeta1 = max(beta1)
            # , bestbeta_factor = beta_factor[which(loglik == max(loglik))[1]]
            # , minbeta_factor = min(beta_factor)
            # , maxbeta_factor = max(beta_factor)
             
            , bestbeta2 = beta2[which(loglik == max(loglik))[1]]
            , minbeta2 = min(beta2)
            , maxbeta2 = max(beta2)
            
            , bestR0before = R0_before[which(loglik == max(loglik))[1]]
            , minR0before = min(R0_before)
            , maxR0before = max(R0_before)
            , bestR0after = R0_after[which(loglik == max(loglik))[1]]
            , minR0after = min(R0_after)
            , maxR0after = max(R0_after)
            
            , bestEinit = E_init[which(loglik == max(loglik))[1]]
            , minEinit = min(E_init)
            , maxEinit = max(E_init)
            , besttinit = t_init[which(loglik == max(loglik))[1]]
            , mintinit = min(t_init)
            , maxtinit = max(t_init)
            , besttinflect = t_inflect[which(loglik == max(loglik))[1]]
            , mintinflect = min(t_inflect)
            , maxtinflect = max(t_inflect)
) %>% 
  mutate(across(contains("tin"), function(x) gsub("^0", " ", format(x, "%d %b")))) %>% 
  mutate_if(is.numeric, function(x) format(round(x, 2), nsmall = 2)) %>%
  # mutate_at(c("bestR0", "minR0", "maxR0", "bestEinit", "minEinit", "maxEinit"), function(x) format(round(x, 1), nsmall = 1)) %>%
  transmute(beta1 = paste0(bestbeta1, " (", minbeta1, "-", maxbeta1, ")")
            # , beta_factor = paste0(bestbeta_factor, " (", minbeta_factor, "-", maxbeta_factor, ")")
            , beta2 = paste0(bestbeta2, " (", minbeta2, "-", maxbeta2, ")")
            
            , R0_before = paste0(bestR0before, " (", minR0before, "-", maxR0before, ")")
            , R0_after = paste0(bestR0after, " (", minR0after, "-", maxR0after, ")")
            
            , E_init = ifelse(minEinit == maxEinit, minEinit, paste0(bestEinit, " (", minEinit, "-", maxEinit, ")"))
            , t_init = ifelse(mintinit == maxtinit, mintinit, paste0(besttinit, " (", mintinit, "-", maxtinit, ")"))
            , t_inflect = paste0(besttinflect, " (", mintinflect, "-", maxtinflect, ")")
  ) %>% 
  mutate(E_init = gsub("^([ ]?[0-9]+)[.]0$", "\\1", E_init)) %>% 
  mutate(E_init = gsub("\\( ", "\\(", E_init))

inflect_tab %>% write_csv(paste0("~/Pasteur/tars/output/Inflection/", experiment_name, "_chiffres.csv"))

res %>% 
  filter(ci == "in_ci") %>% 
  ggpairs(columns = c("loglik", "R0_before", "R0_after")
          , upper = list(continuous = "points")
          , lower = list(continuous = "points"))

ggsave(paste0("~/Pasteur/tars/output/Inflection/", "R0_matrix_plot", experiment_name, ".png"), height = 20, width = 30, units = "cm")







##### simulation plots #####

# (res_ward_prior) # for the ward-level version, but requires different posnegs

res = NULL
for(Eini in c(1)){
  for(analysis in c("beta2", "beta1profile")){
    exp_name = paste0("posneg Inflect ", analysis, " Einit", Eini, " tinflect23")
    
    res_piece = read_delim(paste0("~/tars/output/TOY/cat/seirInflect_", exp_name, ".csv" ), delim = ";") %>% mutate(analysis = analysis)
    if(is.null(res)){
      res = res_piece
    } else {
      res = rbind(res, res_piece)
    }
    
  }
}

pars = 3
ci_interval <- 0.5*qchisq(df=pars,p=0.95)

res <- res %>% 
  mutate(R0_before = beta1*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>% 
  mutate(R0_after = beta2*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>% 
  mutate_at(c("t_inflect", "t_init", "start_tinit"), function(x) as.Date(x, origin = "1970-01-01")) %>% 
  group_by(E_init) %>% 
  mutate(ci_boundary = max(loglik) - ci_interval
         , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")) %>% 
  ungroup


# posneg = read_csv(paste0("~/tars/input/posneg_alltests.csv")) %>%
#   transmute(Date = Date %>% as.numeric, pos, neg, Patients, adm, dd) %>%
#   filter(Date >= as.numeric(as.Date("2020-02-01"))
#          , Date <= as.numeric(as.Date("2020-04-30")))

source("~/pomp_twobeta/seirInflect_source.R")

best_params = res %>% 
  select(seirInflect@params %>% names, starts_with("loglik")) %>% 
  arrange(-loglik) %>% 
  select(-loglik, -loglik.se) %>% 
  {.[1, ]} %>% unlist

approved_params <- res %>% 
  filter(ci == "in_ci") %>% 
  select(-loglik, -loglik.se) %>% 
  arrange(beta1)



#### SIMULATIONS

# extract data in simulate format



obs_df <- seirInflect %>% simulate(params = seirInflect@params %>% replace(c("beta1", "beta2", "t_init", "E_init", "t_inflect")
                                                                           , best_params[c("beta1", "beta2", "t_init", "E_init", "t_inflect")])
                                   , seed = 1, nsim = 1, include.data = T, format = "data.frame") %>% 
  filter(.id == "data") %>% 
  mutate(rep = 0
         , seed_index = as.numeric(NA)
         , id = 0
         , beta1 = as.numeric(NA)
         , beta2 = as.numeric(NA)
         , t_init = as.numeric(NA)
         , E_init = as.numeric(NA)
         , t_inflect = as.numeric(NA)
  )


# simulate single

NSIM = 1000

new_sims <- seirInflect %>% simulate(params = seirInflect@params %>% replace(c("beta1", "beta2", "t_init", "E_init", "t_inflect")
                                                                             , best_params[c("beta1", "beta2", "t_init", "E_init", "t_inflect")])
                                     , seed = 1, nsim = NSIM, include.data = F, format = "data.frame") %>% 
  mutate(rep = as.numeric(.id)
         , seed_index = 0
         , id = seed_index*NSIM*10 + rep
         , beta1 = best_params["beta1"]
         , beta2 = best_params["beta2"]
         , t_init = best_params["t_init"]
         , E_init = best_params["E_init"]
         , t_inflect = best_params["t_inflect"]
)
# 
# new_sims_fixedtinit <- seirPoisson %>% simulate(params = best_params %>% replace("t_init", as.Date("2020-03-05") %>% as.numeric)
#                                  , seed = 1, nsim = NSIM, include.data = F, format = "data.frame") %>% 
#   mutate(rep = as.numeric(.id)
#          , seed_index = 0
#          , id = seed_index*NSIM*10 + rep
#          , beta = best_params["beta"]
#          , t_init = best_params["t_init"])
# 
# new_sims_mod <- seirPoisson %>% simulate(params = best_params %>% replace(c("beta", "t_init"), c(bestbeta/0.7, as.Date("2020-03-05") %>% as.numeric))
#                                                 , seed = 1, nsim = NSIM, include.data = F, format = "data.frame") %>% 
#   mutate(rep = as.numeric(.id)
#          , seed_index = 0
#          , id = seed_index*NSIM*10 + rep
#          , beta = best_params["beta"]
#          , t_init = best_params["t_init"])



# simulate range


NSIM = 1
NSETS = 1000
these_paramsets = sample(nrow(approved_params), size = NSETS, replace = F)
i = 1
#i = 281
for(i in 1:NSETS){
  print(i)
  # this_approved_sims <- simulate(seirInflect, params = approved_params[i, ] %>% unlist
  #                                , seed = i, nsim = NSIM, include.data = F, format = "data.frame")
  
  this_approved_sims_pompobj <- simulate(seirInflect, params = approved_params[these_paramsets[i], names(seirInflect@params)] %>% unlist
                                         , seed = i, nsim = NSIM, include.data = F)
  
  ll <- this_approved_sims_pompobj %>% {dmeasure(.
                                                 , y = obs(seirInflect)
                                                 , x = states(.)
                                                 , params = .@params
                                                 , times = seirInflect@times
                                                 , log = T)} %>% sum
  
  this_approved_sims <- this_approved_sims_pompobj %>% as.data.frame %>% transmute(Date
                                                                                   , rep = i #as.numeric(.id)
                                                                                   , pos = pos
                                                                                   , SAR_numer = SAR_numer
  )
  
  this_approved_sims_meta <- data.frame(rep = i #as.numeric(.id)
                                        , seed_index = these_paramsets[i]
                                        , beta1 = approved_params$beta1[these_paramsets[i]]
                                        , beta2 = approved_params$beta2[these_paramsets[i]]
                                        , t_init = approved_params$t_init[these_paramsets[i]]
                                        , E_init = approved_params$E_init[these_paramsets[i]]
                                        , t_inflect = approved_params$t_inflect[these_paramsets[i]]
                                        , ll = ll)
  
  if(i == 1){
    these_approved_sims = this_approved_sims
    these_approved_sims_meta = this_approved_sims_meta
  } else {
    these_approved_sims = rbind(these_approved_sims, this_approved_sims)
    these_approved_sims_meta = rbind(these_approved_sims_meta, this_approved_sims_meta)
  }
  
}

these_approved_sims %<>% left_join(these_approved_sims_meta) %>%
  mutate(seed_index = rep)

mode_curve <- these_approved_sims %>% filter(ll == max(ll)) %>% 
  transmute(Date, mode = pos)


best_id = these_approved_sims %>% filter(ll == max(ll)) %>% pull(rep) %>% unique %>% {.[1]}
## plot

# SAR_numer_threshold = 0


# obs_df %>% 
#   transmute(Date, Data = pos) %>% 
#   filter(Date >= as.Date("2020-03-01")) %>% 
#   left_join(these_approved_sims %>% 
#               group_by(seed_index, rep) %>% mutate(not_extinct = any(SAR_numer >= SAR_numer_threshold)) %>% filter(not_extinct) %>% ungroup %>% 
#               group_by(Date) %>% 
#               summarise(Simulation_lowCI = quantile(pos, 0.025)
#                         , Simulation_highCI = quantile(pos, 0.975)
#                         , Simulation_median = quantile(pos, 0.5)
#                         , Simulation_max = quantile(pos, 1)
#               )) %>% 
#   pivot_longer(-Date) %>% 
#   mutate(name = factor(name, c("Simulation_max", "Simulation_highCI", "Simulation_median", "Simulation_lowCI", "Data"))) %>% 
#   filter(name %in% c("Data", "Simulation_median", "Simulation_max")) %>% 
#   ggplot(aes(x = as.Date(Date, origin = "1970-01-01"), y = value, linetype = name, colour = name)) + 
#   # scale_linetype_manual(values = c(Data = "solid", Simulation_lowCI = "dashed", Simulation_highCI = "dashed", Simulation_median = "dashed", Simulation_max =  "dashed")) + 
#   # scale_colour_manual(values = c(Data = "black", Simulation_lowCI = "blue", Simulation_highCI = "blue", Simulation_median = "black", Simulation_max =  "lightblue")) + 
#   scale_linetype_manual(values = c(Data = "solid", Simulation_lowCI = "dashed", Simulation_highCI = "dashed", Simulation_median = "solid", Simulation_max =  "solid")) + 
#   scale_colour_manual(values = c(Data = "red", Simulation_lowCI = "blue", Simulation_highCI = "blue", Simulation_median = "blue", Simulation_max =  "lightblue")) + 
#   geom_line(size = 1) + 
#   labs(x = "", y = "Positive tests", colour = "", linetype = "") + 
#   theme_bw() + theme(text = element_text(size = 20), legend.position = c(0.2, 0.85))
# # lims(y = c(0, 20))
# 
# ggsave(paste0("~/Pasteur/tars/output/Figs/", "approved_sims", ".png"), height = 20, width = 20, units = "cm")





# obs_df %>% 
#   transmute(Date, Data = pos) %>% 
#   filter(Date >= as.Date("2020-03-01")) %>% 
#   left_join(these_approved_sims %>% 
#               group_by(seed_index, rep) %>% mutate(not_extinct = any(SAR_numer >= SAR_numer_threshold)) %>% filter(not_extinct) %>% ungroup %>% 
#               group_by(Date) %>% 
#               summarise(lowCI = quantile(pos, 0.025)
#                         , highCI = quantile(pos, 0.975)
#                         , median = quantile(pos, 0.5)
#                         , max = quantile(pos, 1)
#               )) %>% 
#   ggplot(aes(x = as.Date(Date, origin = "1970-01-01"))) + 
#   
#   geom_bar(aes(y = Data, fill = "Data"), stat = "identity") + 
#   geom_ribbon(aes(ymin = lowCI, ymax = highCI), alpha = 0.6, fill = "grey") +
#   geom_line(aes(y = highCI, colour = "CI", linetype = "CI", size = "CI")) +
#   geom_line(aes(y = lowCI, colour = "CI", linetype = "CI", size = "CI")) +
#   geom_line(aes(y = median, colour = "median", linetype = "median", size = "median")) +
#   
#   scale_fill_manual(values = c(Data = "red")) +
#   scale_colour_manual(values = c(median = "black", CI = "grey")) +
#   scale_linetype_manual(values = c(median = "solid", CI = "solid")) +
#   scale_size_manual(values = c(median = 1, CI = 1)) +
#   # guides(fill = "Data") +
#   labs(x = "", y = "Positive tests", fill = "", linetype = "Simulations", size = "Simulations", colour = "Simulations") + 
#   theme_bw() + theme(text = element_text(size = 20)
#                      , legend.position = c(0.2, 0.70)
#                      , legend.background = element_rect(fill="white",
#                                                         size=0.5, linetype="solid"
#                                                         # , colour = "black"
#                                                         )
#                      )
# ggsave(paste0("~/Pasteur/tars/output/Figs/", "approved_sims_updated", ".png"), height = 20, width = 20, units = "cm")

Sys.setlocale("LC_TIME", "English")

SAR_numer_threshold = 3
obs_df %>% 
  transmute(Date, Data = pos) %>% 
  filter(Date >= as.Date("2020-03-01")) %>% 
  left_join(
    these_approved_sims %>%
      # new_sims %>%
      group_by(seed_index, rep) %>% mutate(not_extinct = any(SAR_numer >= SAR_numer_threshold)) %>% filter(not_extinct) %>% ungroup %>% 
      group_by(Date) %>% 
      summarise(lowCI = quantile(pos, 0.025)
                , highCI = quantile(pos, 0.975)
                , median = quantile(pos, 0.5)
                , max = quantile(pos, 1)
      )) %>% 
  left_join(these_approved_sims %>% filter(rep == best_id) %>% transmute(Date, max_logLik = pos)) %>% 
  # left_join(mode_curve) %>% 
  ggplot(aes(x = as.Date(Date, origin = "1970-01-01"))) + 
  
  # geom_bar(aes(y = Data, fill = "Data"), stat = "identity") + 
  geom_ribbon(aes(ymin = lowCI, ymax = highCI), alpha = 0.6, fill = "grey") +
  geom_line(aes(y = highCI, colour = "CI", size = "CI", linetype = "CI")) +
  geom_line(aes(y = lowCI, colour = "CI", size = "CI", linetype = "CI")) +
  # geom_line(aes(y = max_logLik, colour = "bestFit", size = "bestFit", linetype = "bestFit")) +
  geom_line(aes(y = median, colour = "median", size = "median", linetype = "median")) +
  geom_point(aes(y = Data, colour = "Data", size = "Data", linetype = "Data")) + 
  scale_colour_manual(values = c(  Data = "red"   , bestFit = "blue", median = "black" , mode = "blue", CI = "grey")) +
  scale_linetype_manual(values = c(Data = "blank", bestFit = "dashed", median = "dashed", mode = "dotted", CI = "solid")) +
  scale_size_manual(values = c(    Data = 2       , bestFit = 1, median = 1, mode = 1       , CI = 1)) +
  # guides(fill = "Data") +
  labs(x = "", y = "Positive tests", linetype = "Simulations", size = "Simulations", colour = "Simulations") + 
  theme_bw() + theme(text = element_text(size = 20)
                     , legend.position = "none" #c(0.2, 0.70)
                     , legend.background = element_rect(fill="white",
                                                        size=0.5, linetype="solid"
                                                        # , colour = "black"
                     )
  ) + 
  geom_segment(x = as.Date("2020-03-23"), xend = as.Date("2020-03-23")
               , y = 21, yend = 15, arrow = arrow(length = unit(0.1, "inches"), ends = "last")) +
  geom_text(label = expression(t[inflect]), x = as.Date("2020-03-23"), y = 22, size = 10) + 
  coord_cartesian(ylim = c(0, 32))

ggsave(paste0("~/tars/output/Figs/", "new_sims_inflection", ".png"), height = 20, width = 20, units = "cm")


#   geom_bar(aes(y = pos, fill = "pos"), stat = "identity") + 
#   scale_fill_manual(values = c(pos = "red", neg = "blue")) + 
#   geom_ribbon(aes(ymin = low_pos, ymax = high_pos), alpha = 0.8, fill = "pink") + 




# typical sims

set.seed(1)
NTYP = 5
sample_i = sample(nrow(approved_params), NTYP)

for(i in 1:NTYP){
  print(i)
  
  this_typical_sim <- simulate(seirInflect, params = approved_params[sample_i[i], names(seirInflect@params)] %>% unlist
                               , seed = i, nsim = NSIM, include.data = F, format = "data.frame")
  
  this_typical_sim %<>% mutate(rep = as.numeric(.id)
                               , seed_index = i
                               , id = seed_index*NSIM*10 + rep
                               , beta1 = approved_params$beta1[i]
                               , beta2 = approved_params$beta2[i]
                               , t_init = approved_params$t_init[i]
                               , E_init = approved_params$E_init[i]
                               , t_inflect = approved_params$t_inflect[i]
  )
  
  if(i == 1){
    this_typical_sims = this_typical_sim
  } else {
    this_typical_sims = rbind(this_typical_sims, this_typical_sim)
  }
}

this_typical_sims %>% 
  filter(Date >= as.numeric(as.Date("2020-03-01"))) %>% 
  ggplot(aes(x = as.Date(Date, origin = "1970-01-01"), y = pos, colour = as.factor(seed_index))) + 
  geom_line(size = 1, alpha = 0.5) +
  labs(x = "", y = "Positive tests", colour = "", linetype = "") + 
  theme_bw() + theme(text = element_text(size = 20)) + 
  guides(colour = F)

ggsave(paste0("~/Pasteur/tars/output/Figs/", "typical_sims", ".png"), height = 20, width = 20, units = "cm")










# #####
# 
# 
# obs_df %>% 
#   filter(Date >= as.Date("2020-03-01")) %>% 
#   left_join(new_sims %>% 
#               group_by(seed_index, rep) %>% mutate(not_extinct = any(Is >= Is_threshold)) %>% filter(not_extinct) %>% ungroup %>% 
#               group_by(Date) %>% 
#               summarise(low_pos = quantile(pos, 0.025)
#                         , high_pos = quantile(pos, 0.975)
#                         , med_pos = quantile(pos, 0.5)
#                         , top_pos = quantile(pos, 1)
#                         )) %>% 
#   ggplot(aes(x = as.Date(Date, origin = "1970-01-01"))) + geom_bar(aes(y = neg + pos, fill = "neg"), stat = "identity") +
#   geom_bar(aes(y = pos, fill = "pos"), stat = "identity") + 
#   scale_fill_manual(values = c(pos = "red", neg = "blue")) + 
#   geom_ribbon(aes(ymin = low_pos, ymax = high_pos), alpha = 0.8, fill = "pink") + 
#   geom_line(aes(y = med_pos)) +
#   geom_line(aes(y = top_pos), colour = "green", size = 1) +
#   theme_bw() + 
#   labs(x = "", fill = "test", y = "Count", title = "single value")
#   # lims(y = c(0, 20))
# 
# obs_df %>% 
#   filter(Date >= as.Date("2020-03-01")) %>% 
#   left_join(these_approved_sims %>% 
#               group_by(seed_index, rep) %>% mutate(not_extinct = any(Is >= Is_threshold)) %>% filter(not_extinct) %>% ungroup %>% 
#               group_by(Date) %>% 
#               summarise(low_pos = quantile(pos, 0.025)
#                         , high_pos = quantile(pos, 0.975)
#                         , med_pos = quantile(pos, 0.5)
#                         , top_pos = quantile(pos, 1)
#                         )) %>% 
#   ggplot(aes(x = as.Date(Date, origin = "1970-01-01"))) + geom_bar(aes(y = neg + pos, fill = "neg"), stat = "identity") +
#   geom_bar(aes(y = pos, fill = "pos"), stat = "identity") + 
#   scale_fill_manual(values = c(pos = "red", neg = "blue")) + 
#   geom_ribbon(aes(ymin = low_pos, ymax = high_pos), alpha = 0.8, fill = "pink") + 
#   geom_line(aes(y = med_pos)) +
#   geom_line(aes(y = top_pos), colour = "green", size = 1) +
#   theme_bw() + 
#   labs(x = "", fill = "test", y = "Count", title = "range")
#   # lims(y = c(0, 20))

