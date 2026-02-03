library(readxl)
library(tidyverse)
library(lubridate)

# 1. Set the path to your downloaded file
file_path <- "data/usa-gasoline.xls" # Change this to your actual filename

# 2. Read the Excel file
# EIA files usually have headers on the first few rows. 
# We skip the first 2 rows to get the column names correctly.
# Most gasoline data is on "Data 1" (Regular) or "Data 2" (Midgrade)
raw_data <- read_excel(file_path, sheet = "Data 1", skip = 2)

# 3. Clean and Process the Data
gas_prices_clean <- raw_data %>%
  # EIA Date columns are usually the first column
  rename(date = 1) %>% 
  # Select the specific columns for Seattle and West Coast (PADD 5)
  # Note: EIA column names are long, so we use 'contains' to find them
  select(
    date,
    seattle_gas = contains("Seattle"),
    west_coast_gas = contains("West Coast")
  ) %>%
  # Convert date to R date format (sometimes Excel dates import weirdly)
  mutate(date = as.Date(date)) %>%
  # Filter from 2014 to Present
  filter(date >= as.Date("2014-01-01")) %>%
  # Drop any rows where all data is NA (e.g., future dates in the sheet)
  filter(!is.na(seattle_gas) | !is.na(west_coast_gas))

# 4. View the result
print(head(gas_prices_clean))

# 5. Export to CSV
write_csv(gas_prices_clean, "data/usa-gasoline.csv")

message("CSV file has been created successfully!")