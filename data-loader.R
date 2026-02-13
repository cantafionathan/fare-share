library(tidyverse)
library(lubridate)
library(splines)

# Helper function for calculating credible intervals (used in plotting later)
get_intervals <- function(draws, dates, offset) {
  tibble(
    date = dates,
    mean = colMeans(draws) + offset,
    lo90 = apply(draws, 2, quantile, 0.05) + offset,
    lo50 = apply(draws, 2, quantile, 0.25) + offset,
    hi50 = apply(draws, 2, quantile, 0.75) + offset,
    hi90 = apply(draws, 2, quantile, 0.95) + offset
  )
}

load_and_process_data <- function(csv_path, 
                                  treatment_date, 
                                  include_covid = TRUE,
                                  include_unemp = TRUE,
                                  donor_only = FALSE) 
{
  # 1. Load and Shape
  raw_data <- read_csv(csv_path, show_col_types = FALSE)
  treat_date <- as.Date(treatment_date)
  
  wide_data <- raw_data %>%
    arrange(date) %>%
    mutate(time_idx = as.numeric(date - min(date)) / 30)
  
  # 2. Outcome
  if (!"upt_Seattle" %in% names(wide_data)) {
    stop("Error: 'upt_Seattle' column missing from CSV.")
  }
  
  y_all <- log(wide_data$upt_Seattle)
  
  # ----------------------------------------------------------
  # 3. Build Predictors Cleanly
  # ----------------------------------------------------------
  
  # Donor cities (log transformed)
  donor_cols <- names(wide_data) %>%
    str_subset("^upt_") %>%
    setdiff("upt_Seattle")
  
  X_list <- list()
  
  # Add donor cities
  for (col in donor_cols) {
    X_list[[col]] <- log(wide_data[[col]])
  }
  
  # Add local covariates only if not donor_only
  if (!donor_only) {
    
    if (include_unemp && "unemp_rate" %in% names(wide_data)) {
      X_list[["unemp_rate"]] <- wide_data$unemp_rate
    }
    
    if ("gas_price" %in% names(wide_data)) {
      X_list[["gas_price"]] <- wide_data$gas_price
    }
    
    if ("max_temp" %in% names(wide_data)) {
      X_list[["max_temp"]] <- wide_data$max_temp
    }
    
    if ("precip" %in% names(wide_data)) {
      X_list[["precip"]] <- wide_data$precip
    }
    
    if (include_covid && "new_cases" %in% names(wide_data)) {
      X_list[["new_cases"]] <- log1p(wide_data$new_cases)
    }
  }
  
  X_all <- as_tibble(X_list)
  
  # ----------------------------------------------------------
  # 4. Create Splines 
  # ----------------------------------------------------------
  
  B_basis <- bs(wide_data$time_idx, df = 10, degree = 3, intercept = FALSE)
  
  # ----------------------------------------------------------
  # 5. Split Pre / Post
  # ----------------------------------------------------------
  
  pre_idx  <- wide_data$date < treat_date
  post_idx <- wide_data$date >= treat_date
  
  # --- Pre-treatment ---
  keep_pre <- complete.cases(y_all[pre_idx], X_all[pre_idx, ])
  
  y_pre  <- y_all[pre_idx][keep_pre]
  X_pre  <- as.matrix(X_all[pre_idx, ][keep_pre, ])
  B_pre  <- B_basis[pre_idx, ][keep_pre, ]
  dates_pre <- wide_data$date[pre_idx][keep_pre]
  
  # --- Post-treatment ---
  keep_post <- complete.cases(X_all[post_idx, ])
  
  X_post <- as.matrix(X_all[post_idx, ][keep_post, ])
  B_post <- B_basis[post_idx, ][keep_post, ]
  dates_post <- wide_data$date[post_idx][keep_post]
  
  # ----------------------------------------------------------
  # 6. Scale and Center 
  # ----------------------------------------------------------
  
  scaling <- scale(X_pre)
  
  X_pre_scaled  <- as.matrix(scaling)
  X_post_scaled <- scale(
    X_post,
    center = attr(scaling, "scaled:center"),
    scale  = attr(scaling, "scaled:scale")
  )
  
  y_mean <- mean(y_pre)
  y_pre_centered <- y_pre - y_mean
  
  message(paste("Data loaded. Pre-treatment months:", length(y_pre)))
  message(paste("Predictors:", paste(names(X_all), collapse = ", ")))
  
  # ----------------------------------------------------------
  # 7. Return
  # ----------------------------------------------------------
  
  return(list(
    y_pre_centered = y_pre_centered,
    y_mean = y_mean,
    X_pre_scaled = X_pre_scaled,
    X_post_scaled = X_post_scaled,
    B_pre = B_pre,
    B_post = B_post,
    dates_pre = dates_pre,
    dates_post = dates_post,
    y_all = y_all, 
    wide_data = wide_data,
    treatment_date = treat_date,
    y_obs_post = y_all[post_idx][keep_post]
  ))
}
