# ==============================================================================
# ROBUSTNESS TABLE GENERATOR
# ==============================================================================

library(tidyverse)
library(cmdstanr)
library(posterior)
library(loo)

source("data-loader.R")
source("models/blr.R")
source("models/gam.R")
source("models/bssr.R")

csv_path <- "data/my_data.csv"
treatment_date <- "2021-01-01"

# ------------------------------------------------------------------------------
# Helper: Compute Treatment Effect
# ------------------------------------------------------------------------------

compute_ate <- function(fit_obj, dat) {
  
  y_fore_centered <- as_draws_matrix(fit_obj$draws("y_forecast"))
  y_fore_log <- sweep(y_fore_centered, 2, dat$y_mean, "+")
  y_fore_raw <- exp(y_fore_log)
  
  y_obs_raw <- exp(dat$y_obs_post)
  
  lift_matrix <- sweep(-y_fore_raw, 2, y_obs_raw, "+")
  ate_draws <- rowMeans(lift_matrix)
  
  tibble(
    ATE = mean(ate_draws) / 1e6,
    CI_5 = quantile(ate_draws, 0.05) / 1e6,
    CI_95 = quantile(ate_draws, 0.95) / 1e6
  )
}

# ------------------------------------------------------------------------------
# Helper: Extract Diagnostics + Fit
# ------------------------------------------------------------------------------

extract_diagnostics <- function(fit_obj) {
  
  # MCMC summary (exclude generated quantities)
  summ <- fit_obj$summary() %>%
    filter(!str_detect(variable, "log_lik|y_rep|y_forecast"))
  
  max_rhat <- max(summ$rhat, na.rm = TRUE)
  min_ess  <- min(summ$ess_bulk, na.rm = TRUE)
  
  # LOO
  log_lik <- fit_obj$draws("log_lik")
  r_eff <- relative_eff(exp(log_lik))
  loo_res <- loo(log_lik, r_eff = r_eff)
  
  elpd <- loo_res$estimates["elpd_loo", "Estimate"]
  
  pk <- loo_res$diagnostics$pareto_k
  
  tibble(
    Max_Rhat = max_rhat,
    Min_ESS = min_ess,
    ELPD = elpd,
    k_good = sum(pk < 0.7),
    k_ok   = sum(pk >= 0.7 & pk <= 1),
    k_bad  = sum(pk > 1)
  )
}

# ------------------------------------------------------------------------------
# Run All Specifications For One Model
# ------------------------------------------------------------------------------

run_specs <- function(fit_function, model_name) {
  
  specs <- list(
    Baseline = list(include_covid = TRUE,  include_unemp = TRUE,  donor_only = FALSE),
    No_COVID = list(include_covid = FALSE, include_unemp = TRUE,  donor_only = FALSE),
    No_Unemp = list(include_covid = TRUE,  include_unemp = FALSE, donor_only = FALSE),
    Donor_Only = list(include_covid = FALSE, include_unemp = FALSE, donor_only = TRUE)
  )
  
  results <- map_dfr(names(specs), function(spec_name) {
    
    message("Running: ", model_name, " - ", spec_name)
    
    args <- specs[[spec_name]]
    
    dat <- load_and_process_data(
      csv_path = csv_path,
      treatment_date = treatment_date,
      include_covid = args$include_covid,
      include_unemp = args$include_unemp,
      donor_only = args$donor_only
    )
    
    fit <- fit_function(dat)
    
    ate_tbl <- compute_ate(fit, dat)
    diag_tbl <- extract_diagnostics(fit)
    
    bind_cols(
      tibble(
        Model = model_name,
        Specification = spec_name
      ),
      ate_tbl,
      diag_tbl
    )
  })
  
  return(results)
}

# ------------------------------------------------------------------------------
# Run For All Models
# ------------------------------------------------------------------------------

final_results <- bind_rows(
  run_specs(fit_blr,  "BLR"),
  run_specs(fit_gam,  "GAM"),
  run_specs(fit_bssr, "BSSR")
)

# Save CSV
write_csv(final_results, "appendix/robustness_results.csv")

print("Robustness results saved to robustness_results.csv")
print(final_results)
