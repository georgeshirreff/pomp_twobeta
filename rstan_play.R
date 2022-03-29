new_prior_list <- cr0eso::prior_list
# update mean of r0 prior to be 2
new_prior_list$r0_mean <- 2

tmax <- 21
# number of outbreaks
n_outbreaks <- 2
# number of daily cases per outbreak
outbreak_cases <- matrix(c(1,0,0,0,0,1,5,2,6,5,10,11,13,11,9,4,2,6,0,3,1,
                           1,1,0,1,0,0,0,1,3,0,3,2,0,0,3,6,5,6,6,6,10
),ncol=2)
# number of susceptible individuals by location
outbreak_sizes <- c(100,100)


stan_mod <- rstan::stan_model(system.file("stan", "hierarchical_SEIR_incidence_model.stan", package = "cr0eso"))
seir_model_fit
fit <- seir_model_fit(
  stan_model = stan_mod,
  tmax,
  n_outbreaks,
  outbreak_cases,
  outbreak_sizes,
  intervention_switch = FALSE,
  priors = new_prior_list,
  chains = 1)

posts <- rstan::extract(fit$model)
extracted_posts <- hom_extract_posterior_draws(posts) # get object of incidence and zeta
result <- hom_plot_r0_by_location(extracted_posts=extracted_posts)
# plot results
result$plot
