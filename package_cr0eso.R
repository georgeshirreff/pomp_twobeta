library(tidyverse)
library(magrittr)
library(cr0eso)


relative_date_key <- read_csv("~/tars/input/relative_date_key.csv", col_names = c("dates", "key_old", "relDate")) %>%
  mutate(dates = as.Date(dates, format = "%d/%m/%Y"))


posneg_fpt = read_csv("~/tars/input/posneg_fpt.csv") %>% 
  transmute(dates = Date, I = pos) %>%
  filter(dates >= as.Date("2020-03-01")
         , dates <= as.Date("2020-04-30")) %>% 
  left_join(relative_date_key %>% dplyr::select(dates, relDate)) %>% 
  transmute(dates = relDate, I) %>% 
  mutate(cumI = cumsum(I))

posneg_fpt %>% View


#####

posneg_fpt$I[-(1:11)]


posneg_cases <- posneg_fpt$I[-(1:11)] %>% as.matrix
# posneg_cases <- posneg_fpt$I %>% as.matrix

fit <- seir_model_fit(
  stan_model = stan_mod,
  iter = 10000,
  
  # fit_type = "NUTS",
  # fit_type = "VB",
  tmax = nrow(posneg_cases),
  n_outbreaks = 2,
  outbreak_cases = cbind(posneg_cases, posneg_cases),
  outbreak_sizes = c(350, 350),
  intervention_switch = T,
  priors = new_prior_list,
  chains = 1)


posts <- rstan::extract(fit$model)
extracted_posts <- hom_extract_posterior_draws(posts) # get object of incidence and zeta
result <- hom_plot_r0_by_location(extracted_posts=extracted_posts)
# plot results
result$plot

extracted_posts$r0

extracted_posts$params %>% 
  summarise_all(mean)

extracted_posts$r0 %>% 
  filter(location == 1) %>% 
  summarise(r0 = mean(r0))

extracted_posts$r0 %>% 
  filter(location == 1) %>% 
  ggplot(aes(x = r0)) + geom_histogram(aes(y = ..density..)) + 
  labs(x = expression(R[0])) + 
  theme_bw()

ggsave(paste0("~/tars/output/Figs/", "Supp_cr0eso", ".jpg"), 
       , height = 7, width = 10, units = "cm", scale = 1, device = "jpeg", dpi = 600)

