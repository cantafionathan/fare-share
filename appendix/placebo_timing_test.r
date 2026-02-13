# ==============================================================================
# PLACEBO TIMING TEST
# ==============================================================================

library(tidyverse)
library(cmdstanr)
library(posterior)

source("data-loader.R")
source("models/blr.R")
source("models/gam.R")
source("models/bssr.R")

# ------------------------------------------------------------------------------
# SETTINGS
# ------------------------------------------------------------------------------

csv_path <- "data/my_data.csv"
true_treatment_date <- as.Date("2021-01-01")

# Choose placebo grid 
placebo_dates <- seq(
  as.Date("2019-01-01"),
  as.Date("2022-01-01"),
  by = "1 months" # change to 4 or 6 months for different levels of granularity 
)

# ------------------------------------------------------------------------------
# FUNCTION: Compute ATE Given Fit
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# FUNCTION: Compute ATE Given Fit
# ------------------------------------------------------------------------------

compute_ate_from_fit <- function(fit_obj, dat) {
  
  # Counterfactual draws (centered log scale)
  y_fore_centered <- as_draws_matrix(fit_obj$draws("y_forecast"))
  
  # Un-center
  y_fore_log <- sweep(y_fore_centered, 2, dat$y_mean, "+")
  
  # Convert to raw counts
  y_fore_raw <- exp(y_fore_log)
  
  # Observed raw
  y_obs_raw <- exp(dat$y_obs_post)
  
  # Lift matrix: Obs - Pred
  lift_matrix <- sweep(-y_fore_raw, 2, y_obs_raw, "+")
  
  # Average monthly lift per draw
  ate_draws <- rowMeans(lift_matrix)
  
  tibble(
    mean = mean(ate_draws) / 1e6,
    lo90 = quantile(ate_draws, 0.05) / 1e6,
    hi90 = quantile(ate_draws, 0.95) / 1e6,
    # Calculate probability that the effect is positive
    prob_ate_gt_0 = mean(ate_draws > 0)
  )
}

# ------------------------------------------------------------------------------
# MAIN LOOP
# ------------------------------------------------------------------------------

placebo_results <- map_dfr(placebo_dates, function(T0) {
  
  message("Running placebo date: ", T0)
  
  dat <- load_and_process_data(
    csv_path = csv_path,
    treatment_date = as.character(T0),
    include_covid = FALSE,
    include_unemp = FALSE
  )
  
  # --- Fit Models ---
  blr_fit  <- fit_blr(dat)
  gam_fit  <- fit_gam(dat)
  bssr_fit <- fit_bssr(dat)
  
  # --- Compute ATE ---
  blr_ate  <- compute_ate_from_fit(blr_fit, dat)  %>% mutate(Model = "BLR")
  gam_ate  <- compute_ate_from_fit(gam_fit, dat)  %>% mutate(Model = "GAM")
  bssr_ate <- compute_ate_from_fit(bssr_fit, dat) %>% mutate(Model = "BSSR")
  
  bind_rows(blr_ate, gam_ate, bssr_ate) %>%
    mutate(Treatment_Date = T0)
})

# ------------------------------------------------------------------------------
# PLOT ATE 
# ------------------------------------------------------------------------------

p <- ggplot(placebo_results,
            aes(x = Treatment_Date, y = mean, color = Model, fill = Model)) +
  
  geom_ribbon(aes(ymin = lo90, ymax = hi90),
              alpha = 0.15, color = NA) +
  
  geom_line(size = 1) +
  
  geom_vline(xintercept = true_treatment_date,
             linetype = "dashed") +
  
  labs(
    title = "Placebo Timing Test: Estimated ATE by Assumed Treatment Date",
    y = "Average Monthly Lift (Millions of Trips)",
    x = "Assumed Treatment Date"
  ) +
  
  theme_minimal() +
  theme(legend.position = "top")

print(p)

ggsave("appendix/placebo_ate.pdf", p, width = 10, height = 5)

# ------------------------------------------------------------------------------
# PLOT: Probability ATE > 0
# ------------------------------------------------------------------------------

p_prob <- ggplot(placebo_results,
                 aes(x = Treatment_Date, y = prob_ate_gt_0, color = Model)) +
  
  geom_line(size = 1) +
  
  # Add a reference line at 0.95 (standard significance threshold)
  geom_hline(yintercept = 0.95, linetype = "dotted", alpha = 0.5) +
  geom_hline(yintercept = 0.05, linetype = "dotted", alpha = 0.5) +
  
  geom_vline(xintercept = true_treatment_date,
             linetype = "dashed") +
  
  labs(
    title = "Certainty of Effect: Pr(ATE > 0)",
    y = "Posterior Probability",
    x = "Assumed Treatment Date"
  ) +
  
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(legend.position = "top")

print(p_prob)

# Save the probability plot as well
ggsave("appendix/placebo_probs.pdf", p_prob, width = 10, height = 5)

# ------------------------------------------------------------------------------
# SAVE RESULTS 
# ------------------------------------------------------------------------------

write_csv(placebo_results, "appendix/placebo_timing_results.csv")

