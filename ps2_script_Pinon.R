# 3.1 Loading and Saving Data 

library(tidyquant)
library(tidyverse)

# unemp_raw <- tq_get(
#   c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
#   get  = "economic.data",
#   from = "1976-01-01"
# )
# 
# head(unemp_raw)
# 
# dir.create("data", showWarnings = FALSE)
# 
# write_rds(unemp_raw, "data/unemp_raw.rds")
# 
# tail(unemp_raw)
# 
# view(unemp_raw)

unemp_raw = read_rds("data/unemp_raw.rds")


# 4.1 Cleaning up data

unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUR" ~ "Oregon",
      symbol == "CAUR" ~ "California",
      symbol == "TXUR" ~ "Texas",
      symbol == "NYUR" ~ "New York"
    )
  )


# 4.2 Wrangling Data

unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  ) |>
  ungroup()

unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(unemployment_rate_change = unemployment_rate - lag(unemployment_rate)) |>
  ungroup()

view(unemp)


unemp_recent <- unemp |>
  filter(date >= as.Date("2015-01-01"))

view(unemp_recent)


unemp_summary <- unemp |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )

view(unemp_summary)

unemp_recent_summary <- unemp_recent |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )

view(unemp_recent_summary)


# 4.3 Find the Extremes

unemp_top_increases <- unemp |>
  slice_max(unemployment_rate_change, n = 5, with_ties = TRUE)

view(unemp_top_increases)

unemp_top_decreases <- unemp |>
  slice_min(unemployment_rate_change, n = 5, with_ties = TRUE)

view(unemp_top_decreases)


# 4.4 Find the Extremes Excluding 2020

unemp <- unemp |>
  mutate(
    unemployment_rate_change_no_covid = if_else(
      year(date) == 2020,
      NA_real_,
      unemployment_rate_change
    )
  )

unemp_top_increases_no_covid <- unemp |>
  slice_max(unemployment_rate_change_no_covid, n = 5, with_ties = TRUE)

view(unemp_top_increases_no_covid)

unemp_top_decreases_no_covid <- unemp |>
  slice_min(unemployment_rate_change_no_covid, n = 5, with_ties = TRUE)

view(unemp_top_decreases_no_covid)

# 5.1 Produce a Line Plot

ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +       
  labs(title = "Regional Unemployment Rate Over Time", 
       x = "Date", 
       y = "Unemployment Rate",
       color = "Region") +
  theme_minimal()


# 5.2 Producing Individual Line Plots

ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +       
  facet_wrap(~ region) +
  labs(title = "Unemployment Rate Over Time Across Individual Regions", 
       x = "Date", 
       y = "Unemployment Rate",
       color = "Region") +
  theme_minimal()


# 5.3 Producing a Bar Chart

ggplot(unemp_summary, aes(x = fct_reorder(region, mean_rate), y = mean_rate, color = region)) +
  geom_col() +     
  labs(title = "Regional Mean Unemployment Rate Since 1976", 
       x = "Region", 
       y = "Mean Unemployment Rate Since 1976",
       color = "Region") +
  theme_minimal()

# 5.4 My Choice: Bar Chart of Regional Recent Minimums

ggplot(unemp_recent_summary, aes(x = fct_reorder(region, min_rate), y = min_rate, color = region)) +
  geom_col() +     
  labs(title = "Regional Minimum Unemployment Rate Since 2015", 
       x = "Region", 
       y = "Minimum Unemployment Rate Since 2015",
       color = "Region") +
  theme_minimal()

# I chose this final visualization because I thought it would be interesting to look at how low each regions unemployment rate got in the recent time frame, and how they compared to each other.
# We can see that the unemployment rates of California and New York never got as low as the minimums of Oregon, Texas, or the nation at large.


