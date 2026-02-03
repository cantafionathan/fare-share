library(tidyverse)
library(lubridate)
library(jsonlite)

# 1. The function remains the same (fetching daily data)
get_weather <- function(city_name, lat, lon) {
  url <- paste0(
    "https://archive-api.open-meteo.com/v1/archive?",
    "latitude=", lat, "&longitude=", lon,
    "&start_date=2014-01-01",
    "&end_date=", Sys.Date() - 5,
    "&daily=temperature_2m_max,precipitation_sum&timezone=auto"
  )
  
  res <- jsonlite::fromJSON(url)
  
  tibble(
    date = as.Date(res$daily$time),
    city = city_name,
    max_temp_c = res$daily$temperature_2m_max,
    precip_mm = res$daily$precipitation_sum
  )
}

# 2. Fetch for all three cities
seattle_weather   <- get_weather("Seattle", 47.6062, -122.3321)
portland_weather  <- get_weather("Portland", 45.5152, -122.6784)
vancouver_weather <- get_weather("Vancouver", 49.2827, -123.1207)

# 3. Combine and Aggregate Monthly
weather_monthly <- bind_rows(seattle_weather, portland_weather, vancouver_weather) %>%
  mutate(
    # Create a column for the first day of the month
    month_yr = floor_date(date, "month"),
    year = year(date),
    month = month(date, label = TRUE, abbr = TRUE) # Jan, Feb, etc.
  ) %>%
  group_by(city, month_yr, year, month) %>%
  summarise(
    avg_max_temp_c = mean(max_temp_c, na.rm = TRUE),
    total_precip_mm = sum(precip_mm, na.rm = TRUE),
    days_of_rain = sum(precip_mm > 0.5, na.rm = TRUE), # Days with >0.5mm rain
    .groups = "drop"
  )

write_csv(weather_monthly, "data/weather.csv")

# 4. View Results
print(head(weather_monthly))

# 5. Visualization: Monthly Precipitation Trends
ggplot(weather_monthly, aes(x = month_yr, y = total_precip_mm, color = city)) +
  geom_line() +
  labs(
    title = "Monthly Precipitation: Seattle, Portland, Vancouver (2014-Present)",
    x = "Month",
    y = "Total Precipitation (mm)",
    color = "City"
  ) +
  theme_minimal()