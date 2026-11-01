library(tidyverse)
library (fpp3)
retail_raw <- read_csv("data/retail_sales_raw.csv")

head(retail_raw)

retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)

retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")

head(retail_raw_long)

retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)

head(retail_ts)

key_vars(canadian_gas)
index_var(canadian_gas)

can_gas <- canadian_gas |> 
  select(Month, Volume)

has_gaps(can_gas)

head(can_gas)

can_gas_recent <- can_gas |> 
  filter(Month >= yearmonth("1990 Jan"))

head(can_gas_recent)

can_gas_recent <- can_gas_recent |> 
  mutate(year = year(Month),
         month= month(Month, label = TRUE),
         quarter = quarter(Month))

can_gas_recent |> 
  as_tibble() |> 
  group_by(month) |> 
  summarize(avg_volume = mean(Volume))

can_gas_recent |>
  mutate(next_month = Month + 1,
         same_month_last_year = Month - 12) |> 
  head()

can_gas_q <- can_gas |> 
  filter(Month <= yearmonth("2004 Dec")) |> 
  index_by(Quarter = yearquarter(Month)) |> 
  summarize(Volume = sum(Volume))

head(can_gas_q)

can_gas_q |>  autoplot(Volume)
# We sum the monthly values for gas production since gas production is a flow variable and the time frame we are measuring is in quarters.
# We need the quarterly total of gas the amount of gas produced during the month. Taking the mean would give the average monthly output within the quarter.
# Yes, the quarterly series still appears to be seasonal.

retail_ts |> autoplot(sales) + 
  geom_smooth(method = "loess", formula = y ~ x, se = TRUE,
              color = "firebrick", linewidth = 1) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) + theme_minimal()

# The seasonally adjusted plot shows a smoother trend without the seasonal fluctuations that are present in the not seasonally adjusted plot.
# The not seasonally adjusted plot shows clear seasonal spikes or drops in sales once per year. Overall they follow the same trend including recessions.
# 2.7d: I used geom_smooth() to add a loess smoothed line to the plot.
can_gas |> autoplot(Volume) +
  labs(
    title = "Canadian Natural Gas Production (Months)",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Gas production (bcm)", color = NULL
  ) + theme_minimal()

can_gas <- can_gas |> 
  mutate(log_volume = log(Volume))

can_gas |> autoplot(log_volume) +
  labs(
    title = "Canadian Natural Gas Production (log scale)",
    x = NULL, y = "log(billions of cubic meters)"
  ) + theme_minimal()

# The log transformation is useful for stabilizing the variance, however, logs don't really even out the swings for can_gas.
# They just move the problem, so the swings that looked biggest in the middle of the sample now loog biggest at the start.
# The seasonal swings didn't grow in line with the level. Logs still have one real benefit: the slope can be read as a percentage growth rate.

# 3.1c
retail_nsa <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  mutate(log_sales = log(sales))

retail_nsa |>
  autoplot(sales) +
  labs(title = "NSA Retail Sales (level)", x = NULL, y = "Millions of dollars") +
  theme_minimal()

retail_nsa |>
  autoplot(log_sales) +
  labs(title = "NSA Retail Sales (log scale)", x = NULL, y = "log(sales)") +
  theme_minimal()

# The December spikes get bigger as sales grow, so the seasonal swings scale with the elvel.
# The case for logs is stronger here than for Canadian gas production. Retail seasonal
# seasonal swings grow in proportion to the level, which is exactly what logs are designed to handle.

# 3.2
can_gas <- can_gas |> 
  mutate(
    growth_direct = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )

can_gas |> autoplot(growth_direct)

can_gas <- can_gas |> 
  mutate(growth_gap = abs(growth_direct - growth_logdiff))

can_gas |> autoplot(growth_gap) +
  labs(title = "Absolute gap: Exact vs. Log-Difference Growth",
       x = NULL, y = "Percentage points") + theme_minimal()
# The approximation is good for typical months but is noticeablly off in months with
# large seasonal jumps. 

retail_ts <- retail_ts |> 
  group_by_key() |> 
  mutate(growth_mom = 100 * difference(log(sales))) |> 
  ungroup()

retail_ts |> 
  autoplot(growth_mom) +
  labs(
    title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
    x = NULL, y = "Growth rate (%)", color = NULL
  ) +
  theme_minimal()

# NSA Growth is far noiser, since it has larger swings every year (a big jumpt in December and a big drop in January).
# SA growth mostly stays within about -2% to 2% range, while NSA growth rate is mostly the seasonal pattern from 2.7b, and the SA removes that.
# Furthermore, we notice a large rebound in 2020 (COVID) because it was an actual economic shock.

#4.1

retail_ts |> 
  filter(series_id == "RSAFSNA") |> 
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in NSA Retail Sales",
    y = "Millions of dollars"
  ) + theme_minimal()

retail_ts |> 
  filter(series_id == "RSAFS") |> 
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in SA Retail Sales",
    y = "Millions of Dollars"
  ) + theme_minimal()

can_gas |> gg_season(log_volume) +
  labs(
    title = "Seasonal Pattern in Canadian Gas Production (log scale)",
    y = "log(billions of cubic metres"
  ) + theme_minimal()

# Canadian Gas product is higher in the winter months and lower in summer, which makes sense beacuse a lot of natural gas goes to heating.

can_gas |> 
  gg_subseries(log_volume) + 
  labs(
    title = "Subseries Plot: Canadian Gas Production",
    y = "Log(Volume)"
  )

retail_nsa_x13 <- retail_ts |> 
  filter(series_id == "RSAFSNA") |> 
  model(x13 = X_13ARIMA_SEATS(sales)) |> 
  components()

retail_nsa_x13 |> autoplot()

