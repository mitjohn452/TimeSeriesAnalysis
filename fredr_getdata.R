library(fredr)
library(tidyverse)
library(fredr)
library(tidyverse)
fredr_set_key("ce46937f10cba232e2a383786dfac7ba")

series_ids <- c(
  "UNRATE",       # Unemployment Rate
  "SP500",        # S&P 500 - Index
  "AHETPI"       # Avg Hourly Earnings - Total Private
)

# Function to get series data and add series name
get_fred_data <- function(series_id) {
  fredr(
    series_id = series_id,
    observation_start = as.Date("2015-01-01")
  ) |>
    mutate(series_id = series_id)
}

# Pull all data and bind into one dataframe
fred_data <- map_df(series_ids, get_fred_data)


