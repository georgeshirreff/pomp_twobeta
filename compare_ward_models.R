library(readxl)
library(ggplot2)
library(tidyverse)
library(magrittr)
source('~/pomp_twobeta/sim_plot_wards.R')
Sys.setlocale("LC_TIME", "English")


final_date = as.numeric(as.Date("2020-04-30"))

# exp_name = "posneg ALLward Inflect beta2 Einit1"
# pars = 3
# # experiment_name = "posneg ALLward Inflect beta1profile Einit1"

# exp_name = "posneg ALLward Inflect beta2 Einit1 tinflect"
# pars = 4

# exp_name = "posneg ALLward Inflect beta2 Einit1 ABStrans"
# pars = 3

exp_name = "posneg ALLward Inflect beta2 Einit1 ABStrans tinflect23"
pars = 3

ci_interval <- 0.5*qchisq(df=pars,p=0.95)

res_ward_inflect <- rbind(read_delim(paste0("~/tars/output/TOY/cat/seirInflect_", exp_name, ".csv" ), delim = ";") %>% mutate(analysis = "beta2")
                          , read_delim(paste0("~/tars/output/TOY/cat/seirInflect_", gsub("beta2", "beta1profile", exp_name), ".csv"), delim = ";") %>% mutate(analysis = "beta1profile")
)%>%
  mutate(R0_before = beta1*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>%
  mutate(R0_after = beta2*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>%
  mutate(time_phase1 = max(0, t_inflect - t_init)
         , time_phase2 = max(0, final_date - t_inflect)
         , R0 = (R0_before*time_phase1 + R0_after*time_phase2)/(time_phase1 + time_phase2)) %>%
  # mutate(R0 = (R0_before*(t_inflect - t_init) + R0_after*(final_date - t_inflect))/(final_date - t_init)) %>%
  mutate(risk_ratio = beta2/beta1) %>% 
  mutate_at(c("t_inflect", "t_init", "start_tinit"), function(x) as.Date(x, origin = "1970-01-01")) %>%
  group_by(wardCode) %>%
  mutate(ci_boundary = max(loglik) - ci_interval
         , toplot = ifelse(loglik > ci_boundary - ci_interval, "Y", "N")
         , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")) %>%
  ungroup

#### sim #### 

# ward_sim_plot <- pomp_sim_plot(res_ward = res_ward_inflect
#                                , pompModel_source = "~/R code/seirInflect_source_ward.R"
#                                , NSIM = 1000
#                                , SAR_numer_threshold = 3)
# 
# ggsave(ward_sim_plot, filename = paste0("~/tars/output/Wards/", exp_name, "_simplot.png"), units = "cm", width = 40, height = 20)


# res_ward_inflect %>%
#   ggplot(aes(x = t_inflect)) + geom_histogram()

res_ward_inflect %>%
  group_by(wardCode) %>% 
  ggplot(aes(x = beta1, y = loglik, alpha = ci, colour = wardCode)) + geom_point() + 
  geom_hline(aes(yintercept = ci_boundary, colour = wardCode), linetype = "dashed") + 
  facet_wrap(~wardCode, scales = "free_y") +
  scale_alpha_manual(values = c(in_ci = 0.5, out_ci = 0.01)) + 
  # scale_x_continuous(breaks = seq(0, 2, by = 0.5), minor_breaks = seq(0, 10, by = 0.1)) + 
  labs(x = expression(beta[1]), y = "log Likelihood", alpha = "") +
  theme_bw() +
  theme(text = element_text(size = 20)
        , panel.grid.major.x = element_line(size = 1)) + 
  guides(alpha = F)   +
  coord_cartesian(xlim = c(0, 10))

ggsave(paste0("~/tars/output/Wards/", exp_name, "_beta1.png"), units = "cm", width = 30, height = 20)

res_ward_inflect %>%
  group_by(wardCode) %>% 
  ggplot(aes(x = beta2, y = loglik, alpha = ci, colour = wardCode)) + geom_point() + 
  geom_hline(aes(yintercept = ci_boundary, colour = wardCode), linetype = "dashed") + 
  facet_wrap(~wardCode, scales = "free_y") +
  scale_alpha_manual(values = c(in_ci = 0.5, out_ci = 0.01)) + 
  # scale_x_continuous(breaks = seq(0, 2, by = 0.5), minor_breaks = seq(0, 10, by = 0.1)) + 
  labs(x = expression(beta[2]), y = "log Likelihood", alpha = "") +
  theme_bw() +
  theme(text = element_text(size = 20)
        , panel.grid.major.x = element_line(size = 1)) + 
  guides(alpha = F) + 
  coord_cartesian(ylim = c(NA, NA)
                  , xlim = c(NA, NA)
  ) + scale_x_log10()

ggsave(paste0("~/tars/output/Wards/", exp_name, "_beta2.png"), units = "cm", width = 30, height = 20)


res_ward_inflect %>%
  group_by(wardCode) %>% 
  ggplot(aes(x = t_init, y = loglik, alpha = ci, colour = wardCode)) + geom_point() + 
  geom_hline(aes(yintercept = ci_boundary, colour = wardCode), linetype = "dashed") + 
  facet_wrap(~wardCode, scales = "free_y") +
  scale_alpha_manual(values = c(in_ci = 0.5, out_ci = 0.01)) + 
  # scale_x_continuous(breaks = seq(0, 2, by = 0.5), minor_breaks = seq(0, 10, by = 0.1)) + 
  labs(x = expression(t[init]), y = "log Likelihood", alpha = "") +
  theme_bw() +
  theme(text = element_text(size = 20)
        , axis.text.x = element_text(angle = 90)
        , panel.grid.major.x = element_line(size = 1)) + 
  guides(alpha = F) + 
  coord_cartesian(ylim = c(NA, NA)
                  # , xlim = c(0, 2.5)
  ) #+ scale_x_log10()*

ggsave(paste0("~/tars/output/Wards/", exp_name, "_tinit.png"), units = "cm", width = 30, height = 20)

res_ward_inflect %>%
  group_by(wardCode) %>% 
  ggplot(aes(x = t_inflect, y = loglik, alpha = ci, colour = wardCode)) + geom_point() + 
  geom_hline(aes(yintercept = ci_boundary, colour = wardCode), linetype = "dashed") + 
  facet_wrap(~wardCode, scales = "free_y") +
  scale_alpha_manual(values = c(in_ci = 0.5, out_ci = 0.01)) + 
  # scale_x_continuous(breaks = seq(0, 2, by = 0.5), minor_breaks = seq(0, 10, by = 0.1)) + 
  labs(x = expression(t[inflect]), y = "log Likelihood", alpha = "") +
  theme_bw() +
  theme(text = element_text(size = 20)
        , axis.text.x = element_text(angle = 90)
        , panel.grid.major.x = element_line(size = 1)) + 
  guides(alpha = F) + 
  coord_cartesian(ylim = c(NA, NA)
                  # , xlim = c(0, 2.5)
  ) #+ scale_x_log10()*

ggsave(paste0("~/tars/output/Wards/", exp_name, "_tinflect.png"), units = "cm", width = 30, height = 20)

inflect_wtable <- res_ward_inflect %>%  
  group_by(wardCode, E_init) %>% 
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
            , bestt_inflect = t_inflect[loglik == max(loglik)]
            , mint_inflect = min(t_inflect)
            , maxt_inflect = max(t_inflect)
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
  transmute(wardCode
            , E_init
            , beta1 = paste0(bestbeta1, "\n(", minbeta1, "-", maxbeta1, ")")
            # , beta_factor = paste0(bestbeta_factor, " (", minbeta_factor, "-", maxbeta_factor, ")")
            , beta2 = paste0(bestbeta2, "\n(", minbeta2, "-", maxbeta2, ")")
            
            , R0_before = paste0(bestR0before, "\n(", minR0before, "-", maxR0before, ")")
            , R0_after = paste0(bestR0after, "\n(", minR0after, "-", maxR0after, ")")
            
            , R0 = paste0(bestR0, "\n(", minR0, "-", maxR0, ")")
            , risk_ratio = paste0(bestrisk_ratio, "\n(", minrisk_ratio, "-", maxrisk_ratio, ")")
            
            
            , t_inflect = paste0(bestt_inflect, "\n(", mint_inflect, "-", maxt_inflect, ")")
            , t_init = paste0(bestt_init, "\n(", mint_init, "-", maxt_init, ")")
            , AIC
            
  ) %>% 
  mutate(E_init = as.numeric(E_init %>% trimws))

inflect_wtable %>% write_delim(paste0("~/tars/output/Wards/", exp_name, "_wtable.csv"), delim = ";")

#####

# exp_name = "posneg ALLward Refresh Einit1"
# exp_name_prof = "posneg ALLward Refresh betaprofile Einit1"

# exp_name = "posneg ALLwardSide Refresh Einit1"
# exp_name_prof = "posneg ALLwardSide Refresh betaprofile Einit1"

exp_name = "posneg ALLward Refresh Einit1 ABStrans"
exp_name_prof = "posneg ALLward Refresh betaprofile Einit1"


pars = 2
ci_interval <- 0.5*qchisq(df=pars,p=0.95)


res_ward_refresh <- rbind(read_delim(paste0("~/tars/output/TOY/cat/seirRefresh_", exp_name, ".csv" ), delim = ";") %>% mutate(analysis = "beta2")
                          , read_delim(paste0("~/tars/output/TOY/cat/seirRefresh_", exp_name_prof, ".csv" ), delim = ";") %>% mutate(analysis = "beta1profile"))%>% 
  mutate(R0 = beta*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>% 
  mutate_at(c("t_init", "start_tinit"), function(x) as.Date(x, origin = "1970-01-01")) %>% 
  group_by(wardCode) %>% 
  mutate(ci_boundary = max(loglik) - ci_interval
         , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")) %>% 
  ungroup %>% 
  filter(wardCode %in% c("A2", "C0", "C2", "C3"))


#### make simulations happen

ward_sim_plot <- pomp_sim_plot(res_ward = res_ward_refresh
              , pompModel_source = "~/R code/seirRefresh_source_ward.R"
              , NSIM = 1000
              , SAR_numer_threshold = 0
              , calculate_logLik = F)

ggsave(ward_sim_plot, filename = paste0("~/tars/output/Wards/", exp_name, "_simplot.png"), units = "cm", width = 40, height = 20)


res_ward_refresh %>%
  group_by(wardCode) %>% 
  ggplot(aes(x = beta, y = loglik, alpha = ci, colour = wardCode)) + geom_point() + 
  geom_hline(aes(yintercept = ci_boundary, colour = wardCode), linetype = "dashed") + 
  facet_wrap(~wardCode, scales = "free_y") +
  scale_alpha_manual(values = c(in_ci = 0.5, out_ci = 0.01)) + 
  # scale_x_continuous(breaks = seq(0, 2, by = 0.5), minor_breaks = seq(0, 10, by = 0.1)) + 
  labs(x = expression(beta), y = "log Likelihood", alpha = "") +
  theme_bw() +
  theme(text = element_text(size = 20)
        , panel.grid.major.x = element_line(size = 1)) + 
  guides(alpha = F, colour = F) + 
  coord_cartesian(ylim = c(NA, NA)
                  , xlim = c(0, 10)
  )

# ggsave("~/tars/output/Wards/refresh_beta.png", units = "cm", width = 30, height = 20)

ggsave(paste0("~/tars/output/Wards/", exp_name, "_beta.png"), units = "cm", width = 15, height = 20)

res_ward_refresh %>%
  group_by(wardCode) %>% 
  ggplot(aes(x = t_init, y = loglik, alpha = ci, colour = wardCode)) + geom_point() + 
  geom_hline(aes(yintercept = ci_boundary, colour = wardCode), linetype = "dashed") + 
  facet_wrap(~wardCode, scales = "free_y") +
  scale_alpha_manual(values = c(in_ci = 0.5, out_ci = 0.01)) + 
  # scale_x_continuous(breaks = seq(0, 2, by = 0.5), minor_breaks = seq(0, 10, by = 0.1)) + 
  labs(x = expression(t[init]), y = "log Likelihood", alpha = "") +
  theme_bw() +
  theme(text = element_text(size = 20)
        , axis.text.x = element_text(angle = 90)
        , panel.grid.major.x = element_line(size = 1)) + 
  guides(alpha = F, colour = F) + 
  coord_cartesian(ylim = c(NA, NA)
                  # , xlim = c(0, 2.5)
  ) #+ scale_x_log10()

# ggsave("~/tars/output/Wards/refresh_tinit.png", units = "cm", width = 30, height = 20)

ggsave(paste0("~/tars/output/Wards/", exp_name, "_tinit.png"), units = "cm", width = 15, height = 20)

refresh_wtable <- res_ward_refresh %>%  
  group_by(wardCode, E_init) %>% 
  mutate(ci_boundary = max(loglik) - ci_interval
         , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")
         , AIC = 2*pars - 2*max(loglik)) %>% 
  filter(ci == "in_ci") %>%
  summarise(max_loglik = max(loglik)
            , bestbeta = beta[loglik == max(loglik)]
            , minbeta = min(beta)
            , maxbeta = max(beta)
            , bestt_init = t_init[loglik == max(loglik)]
            , mint_init = min(t_init)
            , maxt_init = max(t_init)
            , bestR0 = R0[which(loglik == max(loglik))[1]]
            , minR0 = min(R0)
            , maxR0 = max(R0)
            , AIC = mean(AIC)
  ) %>% 
  ungroup %>% 
  mutate(across(contains("t_in"), function(x) gsub("^0", " ", format(x, "%d %b")))) %>% 
  mutate_if(is.numeric, function(x) format(round(x, 2), nsmall = 2)) %>%
  # mutate_at(c("bestR0", "minR0", "maxR0", "bestEinit", "minEinit", "maxEinit"), function(x) format(round(x, 1), nsmall = 1)) %>%
  transmute(wardCode
            , E_init
            , beta = paste0(bestbeta, "\n(", minbeta, "-", maxbeta, ")")
            # , beta_factor = paste0(bestbeta_factor, " (", minbeta_factor, "-", maxbeta_factor, ")")
            , R0 = paste0(bestR0, "\n(", minR0, "-", maxR0, ")")
            
            , t_init = paste0(bestt_init, "\n(", mint_init, "-", maxt_init, ")")
            , AIC
            
  ) %>% 
  mutate(E_init = as.numeric(E_init %>% trimws))

# refresh_wtable %>% write_csv(paste0("~/tars/output/Wards/", exp_name, "_wtable.csv"))
refresh_wtable %>% write_delim(paste0("~/tars/output/Wards/", exp_name, "_wtable.csv"), delim = ";")





########################## old code #######
res_ward_inflect %>% 
  filter(loglik > ci_boundary) %>% 
  ggplot(aes(x = beta1, y = loglik)) + geom_point() + 
  facet_wrap(.~wardCode, scales = "free_y") + 
  scale_x_log10()

res_ward_refresh %>% 
  ggplot(aes(x = loglik)) + geom_histogram() + 
  facet_wrap(.~wardCode) + 
  scale_y_log10

res_ward_inflect %>% 
  ggplot(aes(x = loglik)) + geom_histogram() + 
  facet_wrap(.~wardCode)

res_ward_inflect %>% 
  filter(wardCode == "C1") %>% 
  pull(loglik)

res_ward_refresh$loglik %>% hist

res_ward_refresh %>% 
  group_by(wardCode) %>% 
  filter(loglik == max(loglik)) %>% View

res_ward_inflect %>% 
  group_by(wardCode) %>% 
  filter(loglik == max(loglik)) %>% pull(wardCode) %>% table
