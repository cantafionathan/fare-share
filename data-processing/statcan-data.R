library(cansim)
library(tidyverse)
library(lubridate)
library(janitor)

# 1. Gasoline
vancouver_gas <- get_cansim("18-10-0001") %>%
  clean_names() %>%
  filter(geo == "Vancouver, British Columbia", 
         str_detect(type_of_fuel, "Regular unleaded")) %>%
  mutate(date = floor_date(date, "month"), gas_price = value, city = "Vancouver") %>%
  select(date, city, gas_price)

# 2. Unemployment
vancouver_unemp <- get_cansim("14-10-0459") %>%
  clean_names() %>%
  filter(geo == "Vancouver, British Columbia", 
         labour_force_characteristics == "Unemployment rate",
         statistics == "Estimate", data_type == "Seasonally adjusted") %>%
  mutate(date = floor_date(date, "month")) %>%
  select(date, unemp_rate = value)

# 3. Ridership (2017-Present) ---
# Table: 23-10-0251 | Region: Prairies, British Columbia and Territories
vancouver_ridership <- get_cansim("23-10-0251") %>%
  clean_names() %>%
  filter(
    geo == "Prairies, British Columbia and Territories",
    total_revenue_and_total_passenger_trips == "Total passenger trips"
  ) %>%
  select(date, value) %>%
  mutate(
    date = floor_date(date, "month"),
    total_upt = as.numeric(value) * 1000000 # Millions to units
  ) %>%
  select(date, total_upt)

# --- 5. Master Join ---
vancouver_monthly <- vancouver_gas %>%
  left_join(vancouver_unemp, by = "date") %>%
  left_join(vancouver_ridership, by = "date") %>%
  filter(year(date) >= 2014)

# --- 6. Verification ---
print("Preview of 2014 Data (Check for non-NA total_upt):")
vancouver_monthly %>% filter(year(date) == 2014) %>% head() %>% print()

print("Summary of NAs in final table:")
colSums(is.na(vancouver_monthly)) %>% print()

# Export
write_csv(vancouver_monthly, "data/statcan.csv")
