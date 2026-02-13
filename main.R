# Load libraries
library(tidyverse)
library(cmdstanr)
library(posterior)
library(loo)

# Source helper scripts
source("data-loader.R")
source("models/blr.R")
source("models/gam.R")
source("models/bssr.R")

# ==============================================================================
# 1. LOAD DATA
# ==============================================================================
# Note: Ensure your 'models/' folder has the .stan files and 'all_data.csv' exists
dat <- load_and_process_data(
  csv_path = "data/my_data.csv", 
  treatment_date = "2021-01-01",
  include_covid = TRUE,
  include_unemp = TRUE,
  donor_only = FALSE
)

# ==============================================================================
# 2. FIT MODELS
# ==============================================================================
message("Fitting BLR...")
blr_fit <- fit_blr(dat)

message("Fitting GAM...")
gam_fit <- fit_gam(dat)

message("Fitting BSTS...")
bssr_fit <- fit_bssr(dat)

# ==============================================================================
# 3. GENERIC PLOTTING FUNCTION
# ==============================================================================
plot_counterfactual <- function(fit_obj, dat, title_text) {
  
  # Extract draws and calc intervals
  draws_pre <- as_draws_matrix(fit_obj$draws("y_rep"))
  df_pre <- get_intervals(draws_pre, dat$dates_pre, dat$y_mean)
  
  draws_for <- as_draws_matrix(fit_obj$draws("y_forecast"))
  df_for <- get_intervals(draws_for, dat$dates_post, dat$y_mean)
  
  # Observed data frame
  df_obs <- tibble(date = dat$wide_data$date, observed = dat$y_all)
  
  p <- ggplot() +
    # 90% CI
    geom_ribbon(data = df_for, aes(date, ymin = lo90, ymax = hi90), fill = "firebrick", alpha = 0.2) +
    # 50% CI
    geom_ribbon(data = df_for, aes(date, ymin = lo50, ymax = hi50), fill = "firebrick", alpha = 0.25) +
    # Mean Forecast
    geom_line(data = df_for, aes(date, mean), color = "firebrick", linetype = "dashed") +
    # 90% CI
    geom_ribbon(data = df_pre, aes(date, ymin = lo90, ymax = hi90), fill = "steelblue", alpha = 0.2) +
    # 50% CI
    geom_ribbon(data = df_pre, aes(date, ymin = lo50, ymax = hi50), fill = "steelblue", alpha = 0.25) +
    # Mean Forecast
    geom_line(data = df_pre, aes(date, mean), color = "steelblue") +
    # Observed
    geom_point(data = df_obs, aes(date, observed), alpha = 0.5) +
    geom_vline(xintercept = dat$treatment_date, linetype = "dotted") +
    labs(title = title_text, y = "Log UPT", x = "Date") +
    theme_minimal()
  
  return(p)
}

# Generate and Print Plots
print(plot_counterfactual(blr_fit, dat, "BLR Counterfactual"))
print(plot_counterfactual(gam_fit, dat, "GAM Counterfactual"))
print(plot_counterfactual(bssr_fit, dat, "BSSR Counterfactual"))

# ==============================================================================
# 4. MCMC DIAGNOSTICS (Rhat & ESS)
# ==============================================================================
get_mcmc_summary <- function(fit_obj, model_name) {
  # 1. Get parameter summary (excluding generated quantities)
  summ <- fit_obj$summary() %>% 
    filter(!str_detect(variable, "log_lik|y_rep|y_forecast"))
  
  # 2. Get Divergences (Corrected Method)
  # diagnostic_summary() returns a list of diagnostics per chain
  diag_summ <- fit_obj$diagnostic_summary(quiet = TRUE)
  total_divs <- sum(diag_summ$num_divergent)
  
  tibble(
    Model = model_name,
    Max_Rhat = max(summ$rhat, na.rm = TRUE),
    Min_Bulk_ESS = min(summ$ess_bulk, na.rm = TRUE),
    Min_Tail_ESS = min(summ$ess_tail, na.rm = TRUE),
    Divergences = total_divs
  )
}

mcmc_table <- bind_rows(
  get_mcmc_summary(blr_fit, "BLR"),
  get_mcmc_summary(gam_fit, "GAM"),
  get_mcmc_summary(bssr_fit, "BSSR")
)

print("--- MCMC Convergence Diagnostics ---")
blr_fit$summary(c("alpha", "beta", "sigma_y"))
gam_fit$summary(c("alpha", "beta", "tau", "sigma_y"))
bssr_fit$summary(c("beta", "sigma_y", "sigma_mu"))
print(mcmc_table)


# ==============================================================================
# 5. MODEL COMPARISON (LOO, RMSE, ATE)
# ==============================================================================
# Helper to extract metrics
evaluate_model <- function(fit_obj, dat, model_name) {
  
  # --- 1. EXISTING METRICS (LOO, RMSE) ---
  
  # LOO
  log_lik <- fit_obj$draws("log_lik")
  r_eff <- relative_eff(exp(log_lik), cores = 2)
  loo_res <- loo(log_lik, r_eff = r_eff)
  pk <- loo_res$diagnostics$pareto_k
  
  # RMSE (Pre-treatment on LOG scale)
  y_rep <- as_draws_matrix(fit_obj$draws("y_rep"))
  y_rep_mean <- colMeans(y_rep)
  rmse_pre <- sqrt(mean((dat$y_pre_centered - y_rep_mean)^2))
  
  # --- 2. CALCULATING LIFT (The Fix) ---
  
  # A. Get draws of the counterfactual (still in centered log units)
  # Dim: [4000 draws x N_post_months]
  y_fore_centered <- as_draws_matrix(fit_obj$draws("y_forecast"))
  
  # B. Un-center to get predicted Log(UPT)
  # We add the pre-treatment mean back
  y_fore_log <- sweep(y_fore_centered, 2, dat$y_mean, "+")
  
  # C. Convert Predictions to RAW scale (UPT counts)
  y_fore_raw <- exp(y_fore_log)
  
  # D. Get Observed data on RAW scale
  # dat$y_obs_post is currently log(UPT), so we exponentiate it
  y_obs_raw <- exp(dat$y_obs_post)
  
  # E. Calculate Absolute Lift (Raw Obs - Raw Pred) for EVERY draw
  # This preserves the uncertainty distribution
  # sweep subtracts vector (y_obs_raw) from matrix rows, so we multiply by -1 to get Obs - Pred
  # Logic: Lift = Obs - Counterfactual
  lift_raw_matrix <- sweep(-y_fore_raw, 2, y_obs_raw, "+")
  
  # F. Calculate Average Monthly Lift per draw
  # This results in a vector of 4000 "Average Monthly Lifts"
  avg_monthly_lift_draws <- rowMeans(lift_raw_matrix)
  
  # G. Summarize Absolute Lift
  abs_lift_mean <- mean(avg_monthly_lift_draws)
  abs_lift_ci   <- quantile(avg_monthly_lift_draws, c(0.05, 0.95))
  
  # --- 3. CALCULATING PERCENTAGE LIFT ---
  
  # It is safer to calculate % lift from the raw aggregates rather than averaging log-diffs
  # Total Observed Ridership (Post-period)
  total_obs <- sum(y_obs_raw)
  
  # Total Predicted Ridership (Post-period) per draw
  total_pred_draws <- rowSums(y_fore_raw)
  
  # % Lift Distribution
  pct_lift_draws <- (total_obs - total_pred_draws) / total_pred_draws
  
  pct_mean <- mean(pct_lift_draws) * 100
  pct_ci   <- quantile(pct_lift_draws, c(0.05, 0.95)) * 100
  
  # Probability of Positive Lift
  prob_positive <- mean(avg_monthly_lift_draws > 0)
  
  # --- 4. RETURN TIBBLE ---
  tibble(
    Model = model_name,
    ELPD = loo_res$estimates["elpd_loo", "Estimate"],
    SE_ELPD = loo_res$estimates["elpd_loo", "SE"],
    `k<0.7` = sum(pk < 0.7),
    `0.7 < k < 1` = sum(pk >= 0.7 & pk <= 1),
    `k > 1` = sum(pk > 1),
    RMSE_Pre = rmse_pre,
    
    # Absolute Monthly Lift (UPT)
    Abs_Lift_Mean = abs_lift_mean / 1000000,
    Abs_Lift_5 = abs_lift_ci[1] / 1000000,
    Abs_Lift_95 = abs_lift_ci[2] / 1000000,
    
    Prob_Positive = prob_positive,
  )
}

# Compile results
results <- bind_rows(
  evaluate_model(blr_fit, dat, "BLR"),
  evaluate_model(gam_fit, dat, "GAM"),
  evaluate_model(bssr_fit, dat, "BSSR") 
) %>% 
  arrange(desc(ELPD))

print("--- Final Model Comparison ---")
print(results)