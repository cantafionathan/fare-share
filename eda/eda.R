library(tidyverse)
library(ggplot2)
library(scales)

# 1. Load Data
# The format is WIDE: Date | upt_Seattle | upt_Portland | ... | Covariates
raw_data <- read_csv("data/my_data.csv", show_col_types = FALSE)

# 2. Reshape Ridership for Plotting (Wide -> Long)
# We need columns: date, city, total_upt
ridership_long <- raw_data %>%
  select(date, starts_with("upt_")) %>%
  pivot_longer(
    cols = -date,
    names_to = "city",
    values_to = "total_upt"
  ) %>%
  mutate(
    # Clean up names: "upt_Seattle" -> "Seattle"
    city = str_remove(city, "upt_"),
    city = str_replace_all(city, "_", " ") # e.g., "San_Francisco" -> "San Francisco"
  )

# ==============================================================================
# PLOT 1: UPT Over Time (All Cities)
# ==============================================================================
ggplot(ridership_long, aes(x = date, y = total_upt, color = city)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  labs(
    title = "Total Monthly Transit Ridership (UPT)",
    subtitle = "Comparing Seattle against Donor Cities",
    x = "Date",
    y = "Trips (Millions)",
    color = "City"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# ==============================================================================
# PLOT 2: Indexed Baseline (Pre-Pandemic = 100)
# ==============================================================================
# Normalize each city so Feb 2020 (or pre-2020 avg) = 100
indexed <- ridership_long %>%
  group_by(city) %>%
  mutate(
    baseline_avg = mean(total_upt[date < as.Date("2020-03-01")], na.rm = TRUE),
    upt_index = (total_upt / baseline_avg) * 100
  ) %>%
  ungroup()

ggplot(indexed, aes(date, upt_index, color = city)) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  # Highlight Seattle
  geom_line(data = filter(indexed, city == "Seattle"), linewidth = 1.2, color = "black") +
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = 100, linetype = "dotted") +
  labs(
    title = "Recovery Index (Pre-Pandemic Average = 100)",
    subtitle = "Black line = Seattle",
    y = "Index",
    x = "Date"
  ) +
  theme_minimal()

# ==============================================================================
# PLOT 3: Year-over-Year Change
# ==============================================================================
yoy <- ridership_long %>%
  group_by(city) %>%
  arrange(date) %>%
  mutate(
    # Calculate % change from same month last year
    upt_yoy = (total_upt / lag(total_upt, 12) - 1)
  ) %>%
  filter(date >= as.Date("2015-01-01")) # Remove first year (NA lags)

ggplot(yoy, aes(date, upt_yoy, color = city)) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.3) +
  geom_line() +
  scale_y_continuous(labels = percent_format()) +
  coord_cartesian(ylim = c(-1, 1)) + # Zoom in (exclude massive 2020 drops)
  labs(
    title = "Year-over-Year % Change in Ridership",
    y = "% Change",
    x = "Date"
  ) +
  theme_minimal()

# ==============================================================================
# PLOT 4: Post-Pandemic Recovery Slope
# ==============================================================================
post_covid <- ridership_long %>%
  filter(date >= as.Date("2021-01-01"))

ggplot(post_covid, aes(date, total_upt, color = city)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  theme_minimal() +
  labs(
    title = "Post-2021 Recovery Trends", 
    subtitle = "Linear trend of ridership volume",
    y = "Trips (Millions)"
  )