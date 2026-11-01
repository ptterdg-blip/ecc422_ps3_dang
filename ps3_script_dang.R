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

# In the seasonal panel, the highest factor will be December, probably around 1.2, which means about 20%
# above trend. Over time, the peaks in December gets smaller in later years and becomes more narrow.

sa_compare <- retail_nsa_x13 |> 
  as_tibble() |> 
  select(Month, mine = season_adjust) |> 
  left_join(
    retail_ts |> 
      filter(series_id == "RSAFS") |> 
      as_tibble() |> 
      transmute(Month, census = sales), 
    by = "Month"
  ) |> 
  mutate(pct_diff = 100 * (log(mine) - log(census)))

sa_compare |> 
  ggplot(aes(x = Month, y = pct_diff)) +
  geom_line() +
  labs(
    title = "My X-13 Seasonal Adjustment Minus the Census Bureau's",
    x = NULL, y = "Difference (%)"
  ) + theme_minimal()

# My adjustment is relatively similar to the Census Bureau's but there are some differences especially around 2020.
# The Census Beureau's adjustment is more aggressive in 2020, and handles COVID months as special outliers. The largest differences are in 2020 and onwards.

can_gas_fit <- can_gas |> 
  model(TSLM(log_volume ~ trend() + season()))

report(can_gas_fit)

can_gas_fit |> 
  tidy() |> 
  select(term, estimate, std.error, statistic, p.value) |> 
  knitr::kable(digits = 3)

# The monthly growth rate is 0.004, so gas production grew by about 0.4% per month on average.
# The most significant seasonal effect is June with a coefficient of -0.251.

can_gas_fit |>
  augment() |> 
  ggplot(aes(x = Month)) + 
  geom_line(aes(y = log_volume, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",
       y = "log(Volume)", color = NULL) + theme_minimal()

# In logs, the fitted trend is a straight line, while the actual series bends (fast growth early, slower later).

can_gas_fit |> gg_tsresiduals()

can_gas_fit |>
  augment() |>
  features(.innov, ljung_box, lag = 24)

# The residuals are not white noise, as the p-value is less than 0.05, indicating that there is still some autocorrelation present in the residuals. This suggests
# that the model may not be fully capturing the underlying patterns in the data, and further model refinement may be necessary.

gas_cv_fc <- can_gas |>
  select(Month, Volume) |>
  stretch_tsibble(.init = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_cv_fc |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))

# The first training window is the 240 months from Jan 1960 to Dec 1979, so the first forecast is for Jan 1980.
# The last training window (ending Feb 2005) forecasts March 2005, which isn't in the data, so the last forecast that can evaluated is for Feb 2005.
# That's 302 one-month ahead forecasts per model. SNAIVE is far more accurate than the trend + seasonal dummies model, and the
# two metrics agree.

errs <- gas_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(can_gas) |> select(Month, actual = Volume),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e) |>
  na.omit()

forecast::dm.test(errs$snaive, errs$tslm, h = 1, power = 1)

# The DM = -31.278, and p-value <2.2e-16, so we reject the null hypothesis that the two models have equal predictive accuracy.
# The SNAIVE model is significantly more accurate than the TSLM model for one month ahead forecasts of Gas production.

gas_cv_fc_roll <- can_gas |> 
  select(Month, Volume) |> 
  slide_tsibble(.size = 240, .step = 1) |> 
  model(snaive = SNAIVE(Volume), 
        tslm = TSLM(log(Volume) ~ trend() + season())) |> 
  forecast(h = 1)

gas_cv_fc_roll |> 
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))

# The MAE and RMSE values are the same as the previous TS-CV results for the random walk.
# The TSLM model improved with an MAE = 2.09 and RMSE = 2.42. This is beacuse each window only covers 20 years.
# The trend is then fit to recent growth instead of being pulled up by the 1960's.





