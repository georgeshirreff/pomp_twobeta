library(pomp)
library(tidyverse)
library(magrittr)
library(ggplot2)

library(EpiEstim)
library(incidence)


relative_date_key <- read_csv("~/tars/input/relative_date_key.csv", col_names = c("dates", "key_old", "relDate")) %>%
  mutate(dates = as.Date(dates, format = "%d/%m/%Y"))

posneg_fpt = read_csv("~/tars/input/posneg_fpt.csv") %>%
  transmute(dates = Date, I = pos) %>%
  filter(dates >= as.Date("2020-03-01")
         , dates <= as.Date("2020-04-30")) %>% 
  left_join(relative_date_key %>% select(dates, relDate)) %>% 
  transmute(dates = relDate, I)
  # transmute(dates = Date %>% as.numeric, I = pos) %>%
  # filter(dates >= as.numeric(as.Date("2020-02-01"))
  #        , dates <= as.numeric(as.Date("2020-04-30")))


plot(as.incidence(posneg_fpt$I, dates = posneg_fpt$dates))




# ee_out <- EpiEstim::estimate_R(incid = posneg_fpt, 
#                      method="parametric_si",
#                      config = make_config(list(
#                        mean_si = 5.8, 
#                        std_si = 1/1.96)))
#   
#   
# plot(ee_out, "R")





## fixing the random seeds
T <- nrow(posneg_fpt)
t_start <- seq(6, T-6) # starting at 2 as conditional on the past observations
t_end <- t_start + 6



MCMC_seed <- 1
overall_seed <- 2
mcmc_control <- make_mcmc_control(seed = MCMC_seed, 
                                  burnin = 1000)
this_config <- make_config(list(mean_si = 5.8, #He et al. serial interval
                           std_si = 1/1.96, #He et al. standard deviation based on 95% CI
                           mcmc_control = mcmc_control,
                           seed = overall_seed, 
                           n1 = 50, 
                           n2 = 50, 
                           t_start = t_start,
                           t_end = t_end
                           ))


ee_out <- EpiEstim::estimate_R(incid = posneg_fpt, 
                               method="parametric_si",
                               config = this_config)

Sys.setlocale("LC_TIME", "English")

plot(ee_out, "R") + 
  theme_bw() + 
  labs(title = "", y = expression(R[t]), x = "relative date") + 
  geom_segment(x = 12, xend = 12, y = 17, yend = 10, col = "red", arrow = arrow(ends = "last", length = unit(0.1, "inches"))) + 
  geom_text(x = 12, y = 20, col = "red", label = expression(t[inflect])) + 
  coord_cartesian(xlim = c(0, 50)) + 
  theme(axis.title.y = element_text(angle = 0, hjust = 0, vjust = 0.5)) + 
  guides(color = "none", fill = "none")


ggsave(paste0("~/tars/output/Figs/", "Supp_EpiEstim_arrow", ".jpg")
       , height = 7, width = 10, units = "cm", scale = 1, device = "jpeg", dpi = 600)

plot(ee_out, "R") + 
  theme_bw() + 
  labs(title = "", y = expression(R[t]), x = "relative date") + 
  coord_cartesian(xlim = c(0, 50)) + 
  theme(axis.title.y = element_text(angle = 0, hjust = 0, vjust = 0.5)) #+ 
  # guides(color = "none", fill = "none")

ggsave(paste0("~/tars/output/Figs/", "Supp_EpiEstim", ".jpg"), 
       , height = 7, width = 10, units = "cm", scale = 1, device = "jpeg", dpi = 600)

citation("EpiEstim")
