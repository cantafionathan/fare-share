library(COVID19)
library(tidyverse)

# 1. Fetch data
us_raw <- covid19("USA", level = 3, start = "2020-01-01", verbose = FALSE)
ca_raw <- covid19("CAN", level = 2, start = "2020-01-01", verbose = FALSE)

# 2. Custom function for safe max
safe_max <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}

# 3. Process US Data - Select only essential columns
us_clean <- us_raw %>%
  filter(
    (administrative_area_level_2 == "Washington" & administrative_area_level_3 == "King") |
      (administrative_area_level_2 == "Oregon" & administrative_area_level_3 == "Multnomah")
  ) %>%
  mutate(region = ifelse(administrative_area_level_3 == "King", "Seattle (King Co)", "Portland (Multnomah Co)")) %>%
  # SELECT ONLY NECESSARY COLUMNS HERE
  select(date, region, confirmed, deaths)

# 4. Process Canada Data - Select only essential columns
ca_clean <- ca_raw %>%
  filter(administrative_area_level_2 == "British Columbia") %>%
  mutate(region = "Vancouver (BC)") %>%
  # SELECT ONLY NECESSARY COLUMNS HERE
  select(date, region, confirmed, deaths)

# 5. Combine (Now bind_rows won't see 'key_local' at all)
covid_final <- bind_rows(us_clean, ca_clean) %>%
  group_by(date, region) %>%
  summarise(
    confirmed = safe_max(confirmed),
    deaths    = safe_max(deaths),
    .groups = "drop"
  ) %>%
  filter(!is.na(confirmed)) %>%
  # Final cleanup of any remaining -Inf or NaN
  mutate(across(c(confirmed, deaths), ~ifelse(is.infinite(.) | is.nan(.), NA, .)))

# 6. Final verification
print("Table Summary:")
print(table(covid_final$region))

head(covid_final)

write_csv(covid_final, "data/covid.csv")
