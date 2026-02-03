library(readxl)
library(tidyverse)
library(lubridate)

# 1. Load the NTD Excel File
file_path_us <- "data/usa-ridership.xlsx" 

# Read the UPT sheet
# We use .name_repair = "minimal" to handle potential duplicate date headers
upt_raw <- read_excel(file_path_us, sheet = "UPT", .name_repair = "minimal")

# 2. Clean and Pivot
us_transit <- upt_raw %>%
  # Filter for Seattle (King Co Metro, Sound Transit) and Portland (TriMet)
  filter(`NTD ID` %in% c("00001", "00040", "00008")) %>%
  
  # 3. Pivot EVERYTHING except the metadata columns
  # This is much safer than using a regex for date patterns
  pivot_longer(
    cols = -c(`NTD ID`, `Legacy NTD ID`, Agency, `Mode/Type of Service Status`, `Reporter Type`, `UACE CD`, `UZA Name`, Mode, TOS, `3 Mode`), 
    names_to = "month_year", 
    values_to = "upt"
  ) %>%
  
  # 4. Flexible Date Parsing
  # 'orders' accounts for various NTD formats: "01/2014", "Jan-14", or "Jan 2014"
  mutate(
    date = parse_date_time(month_year, orders = c("my", "mdy", "ymd", "b y")),
    year = year(date),
    city = ifelse(`NTD ID` == "00008", "Portland", "Seattle")
  ) %>%
  
  # 5. Filter for 2014 - Present and remove rows where date parsing failed
  filter(year >= 2014, !is.na(date)) %>%
  
  # 6. Aggregate by City and Date
  group_by(city, date) %>%
  summarise(
    total_upt = sum(upt, na.rm = TRUE), 
    .groups = "drop"
  )

# 7. Verification: Check the first few rows
print(head(us_transit))

# Save to CSV
write_csv(us_transit, "data/usa-ridership.csv")