library(cmdstanr)

fit_bssr <- function(dat, stan_file = "models/bssr.stan", seed = 547) {
  
  stan_data <- list(
    T = length(dat$y_pre_centered),
    T_new = nrow(dat$X_post_scaled),
    y = dat$y_pre_centered,
    K = ncol(dat$X_pre_scaled),
    X = dat$X_pre_scaled,
    X_new = dat$X_post_scaled
  )
  
  model <- cmdstan_model(stan_file)
  
  fit <- model$sample(
    data = stan_data, 
    seed = seed,
    chains = 4, 
    parallel_chains = 4,
    iter_warmup = 1000, 
    iter_sampling = 1000,
    refresh = 0,
    adapt_delta = 0.95,
    max_treedepth=12
  )
  
  return(fit)
}