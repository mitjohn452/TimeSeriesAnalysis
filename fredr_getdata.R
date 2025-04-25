library(shiny)
library(forecast)
library(ggplot2)
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


# Separate dataframe in to subsets
unrate_data <- fred_data |>
  filter(series_id == "UNRATE") |>
  select(-realtime_start, -realtime_end, -series_id)

sp500_data <- fred_data |>
  filter(series_id == "SP500") |>
  select(-realtime_start, -realtime_end, -series_id)

earnings_data <- fred_data |>
  filter(series_id == "AHETPI") |>
  select(-realtime_start, -realtime_end, -series_id)

ui <- fluidPage(
  titlePanel("Time Series Analysis Final Project"),
  
  sidebarLayout(
    
    # Sidebar with radio buttons
    sidebarPanel(
      
      radioButtons("model", "Choose a Forecast Model:",
                   choices = c("ARIMA" = "arima",
                               "Random Walk" = "rw",
                               "Seasonal Naive" = "snaive"),
                   selected = "arima")
    ),
    
    mainPanel(
      plotOutput("forecastPlot")
    )
  )
)

selected_data <- unrate_data$value # CHANGE FOR DIFFERENT TIME SERIES DATA

server <- function(input, output) {
  
  output$forecastPlot <- renderPlot({
    
    fit <- switch(input$model, 
                 "rw" = rwf(selected_data, drift = FALSE),
                 "arima" = auto.arima(selected_data),
                 "snaive" = snaive(selected_data))
    
    fc <- forecast(fit, h = 12)
    
    autoplot(fc) + 
      ggtitle(paste("Forecast using", toupper(input$model))) +
      theme_classic()
  })
}

shinyApp(ui = ui, server = server)



