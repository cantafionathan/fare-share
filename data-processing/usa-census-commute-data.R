library(tidycensus)
library(tidyverse)

# 1. Setup Parameters
years <- c(2014:2019, 2021:2024)

# Define the regions we want to compare
regions <- list(
  list(state = "WA", county = "King", label = "Seattle (King Co)"),
  list(state = "OR", county = "Multnomah", label = "Portland (Multnomah Co)")
)

# Define the variable mapping for Table B08301
commute_vars <- c(
  total              = "B08301_001",
  drove_alone        = "B08301_003",
  carpool            = "B08301_004",
  public_transit     = "B08301_010",
  taxi_or_ride_share = "B08301_017",
  bicycle            = "B08301_018",
  walked             = "B08301_019",
  other              = "B08301_020",
  work_from_home     = "B08301_021"
)

# 2. Fetch Data using a nested loop for Years and Regions
all_commute_data <- map_dfr(years, function(yr) {
  map_dfr(regions, function(reg) {
    message(paste("Fetching", reg$label, "for year", yr))
    
    get_acs(
      geography = "county",
      state     = reg$state,
      county    = reg$county,
      variables = commute_vars,
      year      = yr,
      survey    = "acs1"
    ) %>%
      mutate(year = yr, region = reg$label)
  })
})

# 3. Clean and Organize
# We keep 'estimate' (the count) and 'moe' (the margin of error)
commute_final <- all_commute_data %>%
  group_by(year, region) %>%
  mutate(
    total_est = estimate[variable == "total"],
    estimate  = estimate / total_est,
    moe       = moe / total_est
  ) %>%
  ungroup() %>%
  select(year, region, variable, estimate, moe) %>%
  arrange(region, year, variable)


# View a sample of the data
print(head(commute_final, 10))

# 4. Export to CSV (Optional)
write_csv(commute_final, "data/usa-commute.csv")

# 5. Visualization: Comparing Public Transit Counts with Error Ba rs
# This shows the actual number of people, including the margin of error
transit_comparison <- commute_final %>%
  filter(variable == "public_transit")

ggplot(transit_comparison, aes(x = year, y = estimate, color = region, fill = region)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = estimate - moe, ymax = estimate + moe), alpha = 0.2, color = NA) +
  geom_point() +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    title = "Public Transit Commuters: Seattle vs. Portland",
    subtitle = "Shaded area represents the Margin of Error (90% confidence)",
    x = "Year",
    y = "Number of Commuters",
    color = "Region",
    fill = "Region"
  ) +
  theme_minimal()