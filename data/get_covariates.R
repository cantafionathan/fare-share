library(tidyverse)
library(lubridate)
library(readxl)
library(jsonlite)
library(COVID19)

# Configuration
START_DATE <- as.Date("2014-01-01")
OUTPUT_FILE <- "data/seattle_covariates.csv"

# ==============================================================================
# 1. WEATHER (Seattle Only - via Open-Meteo API)
# ==============================================================================
message("1. Fetching Seattle Weather (API)...")

url <- paste0(
  "https://archive-api.open-meteo.com/v1/archive?",
  "latitude=47.6062&longitude=-122.3321", # Seattle Coordinates
  "&start_date=2014-01-01",
  "&end_date=", Sys.Date() - 5,
  "&daily=temperature_2m_max,precipitation_sum&timezone=auto"
)

res <- fromJSON(url)

seattle_weather <- tibble(
  date = as.Date(res$daily$time),
  max_temp = res$daily$temperature_2m_max,
  precip = res$daily$precipitation_sum
) %>%
  mutate(date = floor_date(date, "month")) %>%
  group_by(date) %>%
  summarise(
    max_temp = mean(max_temp, na.rm = TRUE),
    precip = sum(precip, na.rm = TRUE),
    .groups = "drop"
  )

# ==============================================================================
# 2. GASOLINE (Seattle Only - via Local XLS)
# ==============================================================================
message("2. Processing Seattle Gasoline Prices...")

# Note: Adjust skip/sheet if your specific file version differs
raw_gas <- read_excel("data/usa-gasoline.xls", sheet = "Data 1", skip = 2)

seattle_gas <- raw_gas %>%
  rename(date = 1) %>% # The date is always col 1
  select(date, seattle_gas = contains("Seattle")) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= START_DATE) %>%
  mutate(date = floor_date(date, "month")) %>%
  group_by(date) %>%
  summarise(gas_price = mean(seattle_gas, na.rm = TRUE), .groups = "drop")

# ==============================================================================
# 3. UNEMPLOYMENT (Seattle Only - via Local CSV)
# ==============================================================================
message("3. Processing Seattle Unemployment...")

unemp_raw <- read_csv("data/seattle-unemployment.csv", show_col_types = FALSE)

seattle_unemp <- unemp_raw %>%
  rename(date = 1, rate = 2) %>% # Rename safely by index
  mutate(date = floor_date(as.Date(date), "month")) %>%
  group_by(date) %>%
  summarise(unemp_rate = mean(rate, na.rm = TRUE), .groups = "drop")

# ==============================================================================
# 4. COVID (Seattle Only - via R Package)
# ==============================================================================
message("4. Fetching Seattle COVID Data...")

covid_raw <- covid19("USA", level = 3, start = "2020-01-01", verbose = FALSE)

seattle_covid <- covid_raw %>%
  filter(administrative_area_level_2 == "Washington", 
         administrative_area_level_3 == "King") %>%
  select(date, confirmed) %>%
  mutate(date = floor_date(date, "month")) %>%
  group_by(date) %>%
  summarise(cases_cumulative = max(confirmed, na.rm = TRUE), .groups = "drop") %>%
  arrange(date) %>%
  mutate(new_cases = cases_cumulative - lag(cases_cumulative, default = 0)) %>%
  mutate(new_cases = ifelse(new_cases < 0, 0, new_cases)) %>%
  select(date, new_cases)

# ==============================================================================
# 5. MERGE AND SAVE
# ==============================================================================
message("5. Merging Covariates...")

# Create a full date sequence to ensure no months are dropped during joins
full_dates <- tibble(
  date = seq(START_DATE, max(seattle_weather$date), by = "month")
)

covariates_master <- full_dates %>%
  left_join(seattle_weather, by = "date") %>%
  left_join(seattle_gas, by = "date") %>%
  left_join(seattle_unemp, by = "date") %>%
  left_join(seattle_covid, by = "date") %>%
  # IMPORTANT: Fill pre-2020 COVID NAs with 0
  replace_na(list(new_cases = 0))

write_csv(covariates_master, OUTPUT_FILE)

message(paste("Success! Covariates saved to:", OUTPUT_FILE))
print(head(covariates_master))