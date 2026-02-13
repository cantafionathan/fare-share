library(tidyverse)
library(lubridate)
library(readxl)

# Configuration
AGENCY_MAP <- c(
  "00001" = "Seattle",        # King County Metro
  "00040" = "Seattle",        # Sound Transit
  "00008" = "Portland",       # TriMet
  "80006" = "Denver",         # RTD
  "90015" = "San_Francisco",  # Muni
  "80001" = "Salt_Lake_City", # UTA
  "50027" = "Minneapolis"     # Metro Transit
)

START_DATE <- as.Date("2014-01-01")

# ==============================================================================
# 1. PROCESS RIDERSHIP (Wide Format)
# ==============================================================================
message("1. Processing Ridership from Excel...")

raw_upt <- read_excel("data/usa-ridership.xlsx", sheet = "UPT", .name_repair = "minimal")

ridership_wide <- raw_upt %>%
  filter(`NTD ID` %in% names(AGENCY_MAP)) %>%
  pivot_longer(
    cols = -c(`NTD ID`, `Legacy NTD ID`, Agency, `Mode/Type of Service Status`, 
              `Reporter Type`, `UACE CD`, `UZA Name`, Mode, TOS, `3 Mode`), 
    names_to = "raw_date", values_to = "upt"
  ) %>%
  mutate(
    # Parse the date
    parsed_date = parse_date_time(raw_date, orders = c("b y", "my", "mdy", "ymd")),
    # Force everything to the 1st of the month to match covariates
    date = floor_date(parsed_date, "month"),
    city = AGENCY_MAP[`NTD ID`]
  ) %>%
  filter(date >= START_DATE, !is.na(date)) %>%
  # Sum agencies (e.g., King Co + Sound Transit)
  group_by(date, city) %>%
  summarise(total_upt = sum(upt, na.rm = TRUE), .groups = "drop") %>%
  # Pivot Wide: One row per month, columns for each city
  pivot_wider(names_from = city, values_from = total_upt, names_prefix = "upt_")

# ==============================================================================
# 2. LOAD COVARIATES
# ==============================================================================
message("2. Loading Pre-fetched Covariates...")

if (!file.exists("data/seattle_covariates.csv")) {
  stop("Error: 'data/seattle_covariates.csv' not found. Please run 'get_covariates.R' first.")
}

covariates <- read_csv("data/seattle_covariates.csv", show_col_types = FALSE) %>%
  # Ensure covariate dates are also strictly Date objects
  mutate(date = as.Date(date))

# ==============================================================================
# 3. MASTER JOIN
# ==============================================================================
message("3. Creating Master Dataset...")

master_data <- ridership_wide %>%
  left_join(covariates, by = "date") %>%
  # Ensure we only keep rows where we have the target variable (Seattle Ridership)
  filter(!is.na(upt_Seattle)) %>%
  arrange(date)

# ==============================================================================
# 4. EXPORT
# ==============================================================================
write_csv(master_data, "data/my_data.csv")

message("------------------------------------------------")
message("Success! 'my_data.csv' created.")
message(paste("Rows:", nrow(master_data)))
message(paste("Date Range:", min(master_data$date), "to", max(master_data$date)))
message("Columns:")
print(names(master_data))
message("------------------------------------------------")