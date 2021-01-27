experiment_name = "posneg Inflect"
experiment_name = "posneg Inflect tinitFeb27"
experiment_name = "posneg Inflect tinitFeb20"

pars = 4
ci_interval <- 0.5*qchisq(df=pars,p=0.95)

res <-  read_csv(paste0("~/Pasteur/tars/output/TOY/cat/seirInflect_", experiment_name, ".csv" )) %>% 
  mutate_at(c("t_inflect", "t_init", "start_tinit"), function(x) as.Date(x, origin = "1970-01-01")) %>% 
  mutate(R0_before = beta1*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>% 
  mutate(R0_after = beta1*beta_factor*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>% 
  mutate(ci_boundary = max(loglik) - ci_interval
         , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci"))


library(GGally)

res %>% 
  filter(ci == "in_ci") %>% 
  ggpairs(columns = c("loglik", "beta1", "beta_factor", "t_inflect", "E_init")
          , upper = list(continuous = "points")
          , lower = list(continuous = "points"))

ggsave(paste0("~/Pasteur/tars/output/Inflection/", "matrix_plot", experiment_name, ".png"), height = 20, width = 30, units = "cm")


res %>% 
  filter(ci == "in_ci") %>% 
  ggpairs(columns = c("loglik", "R0_before", "R0_after")
          , upper = list(continuous = "points")
          , lower = list(continuous = "points"))

ggsave(paste0("~/Pasteur/tars/output/Inflection/", "R0_matrix_plot", experiment_name, ".png"), height = 20, width = 30, units = "cm")




res %>% 
  ggplot(aes(x = beta1, y = loglik)) + geom_point()

res %>% 
  ggplot(aes(x = beta_factor, y = loglik)) + geom_point()

res %>% 
  ggplot(aes(x = t_Inflect, y = loglik)) + geom_point()

res %>% 
  ggplot(aes(x = E_init, y = loglik)) + geom_point()

res %>% 
  filter(E_init < 500) %>% 
  ggplot(aes(x = E_init, y = loglik)) + geom_point()

seirInflect


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



obs_df <- seirInflect %>% simulate(params = seirInflect@params %>% replace(c("beta1", "beta_factor", "t_inflect", "E_init")
                                                                           , best_params[c("beta1", "beta_factor", "t_inflect", "E_init")])
                                   , seed = 1, nsim = 1, include.data = T, format = "data.frame") %>% 
  filter(.id == "data") %>% 
  mutate(rep = 0
         , seed_index = as.numeric(NA)
         , id = 0
         , beta1 = as.numeric(NA)
         , beta_factor = as.numeric(NA)
         , t_Inflect = as.numeric(NA)
         , E_init = as.numeric(NA)
  )


# simulate single

NSIM = 1000

new_sims <- seirInflect %>% simulate(params = seirInflect@params %>% replace(c("beta1", "beta_factor", "t_Inflect", "E_init")
                                                                             , best_params[c("beta1", "beta_factor", "t_Inflect", "E_init")])
                                     , seed = 1, nsim = NSIM, include.data = F, format = "data.frame") %>% 
  mutate(rep = as.numeric(.id)
         , seed_index = 0
         , id = seed_index*NSIM*10 + rep
         , beta = best_params["beta"]
         , t_init = best_params["t_init"]
         , E_init = best_params["E_init"]
, beta1 = best_params["beta1"]
, beta_factor = best_params["beta_factor"]
, t_Inflect = best_params["t_Inflect"]
, E_init = best_params["E_init"]
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

nrow(approved_params)
NSIM = 1
i = 1
#i = 281
for(i in 1:nrow(approved_params)){
  print(i)
  # this_approved_sims <- simulate(seirInflect, params = approved_params[i, ] %>% unlist
  #                                , seed = i, nsim = NSIM, include.data = F, format = "data.frame")
  
  this_approved_sims_pompobj <- simulate(seirInflect, params = approved_params[i, ] %>% unlist
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
                                        , seed_index = i
                                        , beta = approved_params$beta[i]
                                        , t_init = approved_params$t_init[i]
                                        , E_init = approved_params$E_init[i]
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

## plot

SAR_numer_threshold = 0


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

SAR_numer_threshold = 3
obs_df %>% 
  transmute(Date, Data = pos) %>% 
  filter(Date >= as.Date("2020-03-01")) %>% 
  left_join(
    # these_approved_sims %>%
      new_sims %>%
      group_by(seed_index, rep) %>% mutate(not_extinct = any(SAR_numer >= SAR_numer_threshold)) %>% filter(not_extinct) %>% ungroup %>% 
      group_by(Date) %>% 
      summarise(lowCI = quantile(pos, 0.025)
                , highCI = quantile(pos, 0.975)
                , median = quantile(pos, 0.5)
                , max = quantile(pos, 1)
      )) %>% 
  # left_join(mode_curve) %>% 
  ggplot(aes(x = as.Date(Date, origin = "1970-01-01"))) + 
  
  # geom_bar(aes(y = Data, fill = "Data"), stat = "identity") + 
  geom_ribbon(aes(ymin = lowCI, ymax = highCI), alpha = 0.6, fill = "grey") +
  geom_line(aes(y = highCI, colour = "CI", size = "CI", linetype = "CI")) +
  geom_line(aes(y = lowCI, colour = "CI", size = "CI", linetype = "CI")) +
  geom_line(aes(y = median, colour = "median", size = "median", linetype = "median")) +
  # geom_line(aes(y = mode, colour = "mode", size = "mode", linetype = "mode")) +
  geom_point(aes(y = Data, colour = "Data", size = "Data", linetype = "Data")) + 
  
  # scale_fill_manual(values = c(Data = "red")) +
  # scale_colour_manual(values = c(  Data = "red"   , mode = "black" , CI = "grey")) +
  # scale_linetype_manual(values = c(Data = "blank", mode = "dashed", CI = "solid")) +
  # scale_size_manual(values = c(    Data = 2       , mode = 1       , CI = 1)) +
  scale_colour_manual(values = c(  Data = "red"   , median = "black" , mode = "blue", CI = "grey")) +
  scale_linetype_manual(values = c(Data = "blank", median = "dashed", mode = "dotted", CI = "solid")) +
  scale_size_manual(values = c(    Data = 2       , median = 1, mode = 1       , CI = 1)) +
  # guides(fill = "Data") +
  labs(x = "", y = "Positive tests", linetype = "Simulations", size = "Simulations", colour = "Simulations") + 
  theme_bw() + theme(text = element_text(size = 20)
                     , legend.position = c(0.2, 0.70)
                     , legend.background = element_rect(fill="white",
                                                        size=0.5, linetype="solid"
                                                        # , colour = "black"
                     )
  )
ggsave(paste0("~/Pasteur/tars/output/Figs/", "new_sims_inflection", ".png"), height = 20, width = 20, units = "cm")


#   geom_bar(aes(y = pos, fill = "pos"), stat = "identity") + 
#   scale_fill_manual(values = c(pos = "red", neg = "blue")) + 
#   geom_ribbon(aes(ymin = low_pos, ymax = high_pos), alpha = 0.8, fill = "pink") + 




# typical sims

set.seed(1)
NTYP = 5
sample_i = sample(nrow(approved_params), NTYP)

for(i in 1:NTYP){
  print(i)
  
  this_typical_sim <- simulate(seirInflect, params = approved_params[sample_i[i], ] %>% unlist
                               , seed = i, nsim = NSIM, include.data = F, format = "data.frame")
  
  this_typical_sim %<>% mutate(rep = as.numeric(.id)
                               , seed_index = i
                               , id = seed_index*NSIM*10 + rep
                               , beta = approved_params$beta[i]
                               , t_init = approved_params$t_init[i]
                               , E_init = approved_params$E_init[i]
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

