library(cansim)
library(tidyverse)
library(janitor)

# --- 1. Fetch 2021 Commute Data (Table 98-10-0457) ---
# This table is great because it includes BOTH Mode of Transport and WFH status
van_2021_raw <- get_cansim("98-10-0457") %>%
  clean_names() %>%
  filter(str_detect(geo, "Vancouver"))

van_2021_clean <- van_2021_raw %>%
  # Filter for the 'Total' age group and 'Both sexes' to avoid duplicates
  filter(str_detect(age_15_years_and_over, "^Total"),
         str_detect(gender, "^Total")) %>%
  # Standardize the labels
  mutate(variable = case_when(
    str_detect(main_mode_of_commuting, "Worked at home") ~ "work_from_home",
    str_detect(main_mode_of_commuting, "as a driver") ~ "drove_alone",
    str_detect(main_mode_of_commuting, "as a passenger") ~ "carpool",
    str_detect(main_mode_of_commuting, "Public transit") ~ "public_transit",
    str_detect(main_mode_of_commuting, "Walked") ~ "walked",
    str_detect(main_mode_of_commuting, "Bicycle") ~ "bicycle",
    str_detect(main_mode_of_commuting, "Other method") ~ "other",
    str_detect(main_mode_of_commuting, "^Total - Main mode") ~ "total",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(variable)) %>%
  group_by(variable) %>%
  summarise(estimate = sum(value, na.rm = TRUE)) %>%
  mutate(year = 2021, region = "Vancouver", moe = NA)

# --- 2. Fetch 2016 Commute Data (Table 98-400-X2016391) ---
# This is a specific 2016 Commuting table
van_2016_raw <- get_cansim("98-400-X2016391") %>%
  clean_names() %>%
  filter(str_detect(geo, "Vancouver"))

van_2016_clean <- van_2016_raw %>%
  # Filter for 'Both sexes' and 'Total' to avoid double counting
  filter(str_detect(sex, "Both sexes"),
         str_detect(age_groups, "Total")) %>%
  mutate(variable = case_when(
    str_detect(main_mode_of_commuting, "Worked at home") ~ "work_from_home",
    str_detect(main_mode_of_commuting, "as a driver") ~ "drove_alone",
    str_detect(main_mode_of_commuting, "as a passenger") ~ "carpool",
    str_detect(main_mode_of_commuting, "Public transit") ~ "public_transit",
    str_detect(main_mode_of_commuting, "Walked") ~ "walked",
    str_detect(main_mode_of_commuting, "Bicycle") ~ "bicycle",
    str_detect(main_mode_of_commuting, "Other method") ~ "other",
    str_detect(main_mode_of_commuting, "Total - Main mode") ~ "total",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(variable)) %>%
  group_by(variable) %>%
  summarise(estimate = sum(value, na.rm = TRUE)) %>%
  mutate(year = 2016, region = "Vancouver", moe = NA)

# --- 3. Final Combine ---
vancouver_commute_final <- bind_rows(van_2016_clean, van_2021_clean) %>%
  select(year, region, variable, estimate, moe)

print(vancouver_commute_final)