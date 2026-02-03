library(tidyverse)
library(lubridate)
library(zoo)

# 1. Load all data
statcan <- read_csv("data/statcan.csv")
mobility <- read_csv("data/google_mobility.csv")

usa_gasoline <- read_csv("data/usa-gasoline.csv")
usa_ridership <- read_csv("data/usa-ridership.csv")
portland_unemp <- read_csv("data/portland-unemployment.csv")
seattle_unemp <- read_csv("data/seattle-unemployment.csv")
usa_commute <- read_csv("data/usa-commute.csv")

covid <- read_csv("data/covid.csv")
weather <- read_csv("data/weather.csv")

# --- 1. Process USA Gasoline (Weekly -> Monthly) ---
usa_gas_monthly <- usa_gasoline %>%
  mutate(date = floor_date(date, "month")) %>%
  group_by(date) %>%
  summarise(
    Seattle = mean(seattle_gas, na.rm = TRUE),
    Portland = mean(west_coast_gas, na.rm = TRUE)
  ) %>%
  pivot_longer(-date, names_to = "city", values_to = "gas_price")

# --- 2. Process USA Ridership (Standardize Date) ---
usa_ridership_monthly <- usa_ridership %>%
  mutate(date = floor_date(as.Date(date), "month")) %>%
  select(date, city, total_upt)

# --- 3. Process USA Unemployment (Combine & Rename) ---
usa_unemp_monthly <- bind_rows(
  seattle_unemp %>% rename(date = observation_date, unemp_rate = 2) %>% mutate(city = "Seattle"),
  portland_unemp %>% rename(date = observation_date, unemp_rate = 2) %>% mutate(city = "Portland")
) %>% mutate(date = floor_date(date, "month"))

# --- 4. Process COVID (Daily/Cumulative -> Monthly New Cases) ---
covid_monthly <- covid %>%
  mutate(
    date = floor_date(date, "month"),
    city = case_when(
      str_detect(region, "Seattle") ~ "Seattle",
      str_detect(region, "Portland") ~ "Portland",
      str_detect(region, "Vancouver") ~ "Vancouver",
      TRUE ~ region
    )
  ) %>%
  group_by(date, city) %>%
  summarise(new_cases = max(confirmed, na.rm = TRUE) - min(confirmed, na.rm = TRUE), .groups = "drop") %>%
  mutate(new_cases = ifelse(new_cases < 0, 0, new_cases))

# --- 5. Process USA Commute (Yearly -> Monthly Pivot) ---
usa_commute_wide <- usa_commute %>%
  mutate(city = case_when(
    str_detect(region, "Seattle") ~ "Seattle",
    str_detect(region, "Portland") ~ "Portland",
    TRUE ~ region
  )) %>%
  select(year, city, variable, estimate) %>%
  pivot_wider(names_from = variable, names_prefix = "pct_", values_from = estimate)

# --- 6. Process Google Mobility (Daily -> Monthly Average) ---
mobility_monthly <- mobility %>%
  mutate(
    date = floor_date(as.Date(date), "month"),
    # Standardizing city names based on the sub_regions
    city = case_when(
      str_detect(sub_region_2, "King") ~ "Seattle",
      str_detect(sub_region_2, "Multnomah") ~ "Portland",
      str_detect(sub_region_2, "Vancouver") ~ "Vancouver",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(city)) %>%
  group_by(date, city) %>%
  summarise(
    across(ends_with("baseline"), mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Clean up the column names to be shorter
  rename_with(~str_replace(., "_percent_change_from_baseline", "_mob"), ends_with("baseline"))

# --- 7. The Master Join ---
grid <- expand_grid(
  date = seq(as.Date("2014-01-01"), as.Date("2025-12-01"), by = "month"),
  city = c("Seattle", "Portland", "Vancouver")
)

master_df <- grid %>%
  left_join(weather %>% select(month_yr, city, avg_max_temp_c, total_precip_mm), by = c("date" = "month_yr", "city")) %>%
  left_join(covid_monthly, by = c("date", "city")) %>%
  left_join(usa_ridership_monthly, by = c("date", "city")) %>%
  left_join(usa_gas_monthly, by = c("date", "city")) %>%
  left_join(usa_unemp_monthly, by = c("date", "city")) %>%
  left_join(mobility_monthly, by = c("date", "city")) %>% # Joined Mobility here
  left_join(statcan, by = c("date", "city"), suffix = c("", "_can")) %>%
  mutate(
    gas_price = coalesce(gas_price, gas_price_can),
    unemp_rate = coalesce(unemp_rate, unemp_rate_can),
    total_upt = coalesce(total_upt, total_upt_can)
  ) %>%
  select(-ends_with("_can")) %>%
  mutate(year = year(date)) %>%
  left_join(usa_commute_wide, by = c("year", "city"))

# --- 8. Final Polish: Interpolation ---
master_df <- master_df %>%
  group_by(city) %>%
  arrange(date) %>%
  mutate(across(starts_with("pct_"), ~na.approx(., na.rm = FALSE))) %>%
  ungroup()

# Export
write_csv(master_df, "total_data.csv")