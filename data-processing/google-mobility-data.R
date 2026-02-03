# Install and load the necessary package
# install.packages("tidyverse")
library(tidyverse)

# 1. Define the filenames (make sure these are in your working directory)
us_files <- c("data/2020_US_Region_Mobility_Report.csv", 
              "data/2021_US_Region_Mobility_Report.csv", 
              "data/2022_US_Region_Mobility_Report.csv")

ca_files <- c("data/2020_CA_Region_Mobility_Report.csv", 
              "data/2021_CA_Region_Mobility_Report.csv", 
              "data/2022_CA_Region_Mobility_Report.csv")

# 2. Function to read and filter data efficiently
filter_mobility <- function(file_list, region1, region2) {
  map_df(file_list, ~read_csv(.x, show_col_types = FALSE) %>% 
           filter(sub_region_1 == region1, 
                  sub_region_2 == region2))
}

# 3. Filter for each city
# Seattle is in King County, Washington
seattle_data <- filter_mobility(us_files, "Washington", "King County")

# Portland is in Multnomah County, Oregon
portland_data <- filter_mobility(us_files, "Oregon", "Multnomah County")

# Vancouver is in the Greater Vancouver Regional District, BC
vancouver_data <- filter_mobility(ca_files, "British Columbia", "Metro Vancouver")

# 4. Combine all into one dataframe
combined_mobility <- bind_rows(
  seattle_data %>% mutate(city = "Seattle"),
  portland_data %>% mutate(city = "Portland"),
  vancouver_data %>% mutate(city = "Vancouver")
)

# 5. View or Save the results
head(combined_mobility)
write_csv(combined_mobility, "data/google_mobility.csv")