exp_name = "posneg ALLward Inflect beta2 Einit1 hypertinflect"
pars = 4


ci_interval <- 0.5*qchisq(df=pars,p=0.95)

res_ward_inflect <- rbind(read_delim(paste0("~/tars/output/TOY/cat/seirInflect_", exp_name, ".csv" ), delim = ";") %>% mutate(analysis = "beta2")
                          # , read_delim(paste0("~/tars/output/TOY/cat/seirInflect_", gsub("beta2", "beta1profile", exp_name), ".csv"), delim = ";") %>% mutate(analysis = "beta1profile")
)%>% 
  mutate(t_inflect_lo = "2020-03-19" %>% as.Date %>% as.numeric
         , t_inflect_hi = "2020-03-23" %>% as.Date %>% as.numeric
         , t_inflect = t_inflect_lo*(1-hyper) + t_inflect_hi*(hyper)) %>% 
  mutate(R0_before = beta1*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>% 
  mutate(R0_after = beta2*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>% 
  mutate(time_phase1 = max(0, t_inflect - t_init)
         , time_phase2 = max(0, final_date - t_inflect)
         , R0 = (R0_before*time_phase1 + R0_after*time_phase2)/(time_phase1 + time_phase2)) %>% 
  # mutate(R0 = (R0_before*(t_inflect - t_init) + R0_after*(final_date - t_inflect))/(final_date - t_init)) %>% 
  mutate_at(c("t_inflect", "t_init", "start_tinit"), function(x) as.Date(x, origin = "1970-01-01")) %>% 
  group_by(wardCode) %>% 
  mutate(ci_boundary = max(loglik) - ci_interval
         , toplot = ifelse(loglik > ci_boundary - ci_interval, "Y", "N")
         , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")) %>% 
  ungroup




exp_name = "posneg ALLward Inflect beta2 Einit1 ABStrans"
exp2_name = gsub("beta2", "beta1profile", exp_name)

# exp_name = "posneg ALLward Refresh Einit1 ABStrans"
# exp2_name = "posneg ALLward Refresh betaprofile Einit1"

pars = 3


ci_interval <- 0.5*qchisq(df=pars,p=0.95)

res_ward_inflect <- rbind(read_delim(paste0("~/tars/output/TOY/cat/seirInflect_", exp_name, ".csv" ), delim = ";") %>% mutate(analysis = "beta2")
                          , read_delim(paste0("~/tars/output/TOY/cat/seirInflect_", exp2_name, ".csv"), delim = ";") %>% mutate(analysis = "beta1profile")
)%>% 
  mutate(R0_before = beta1*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>% 
  mutate(R0_after = beta2*(psi*(1/gamma*epsilon + 1/(delta)) + kappa1*(1-psi)*(1/(gamma*kappa2)*epsilon + 1/(delta*kappa3)))) %>% 
  mutate(time_phase1 = max(0, t_inflect - t_init)
         , time_phase2 = max(0, final_date - t_inflect)
         , R0 = (R0_before*time_phase1 + R0_after*time_phase2)/(time_phase1 + time_phase2)) %>% 
  # mutate(R0 = (R0_before*(t_inflect - t_init) + R0_after*(final_date - t_inflect))/(final_date - t_init)) %>% 
  mutate_at(c("t_inflect", "t_init", "start_tinit"), function(x) as.Date(x, origin = "1970-01-01")) %>% 
  group_by(wardCode) %>% 
  mutate(ci_boundary = max(loglik) - ci_interval
         , toplot = ifelse(loglik > ci_boundary - ci_interval, "Y", "N")
         , ci =  ifelse(loglik > max(loglik) - ci_interval, "in_ci", "out_ci")) %>% 
  ungroup


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
  scale_x_log10() + 
  coord_cartesian(xlim = c(0.1, 50))
