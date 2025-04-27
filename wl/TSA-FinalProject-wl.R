library(shiny)
library(shinydashboard)
library(forecast)
library(ggplot2)
library(fredr)
library(tidyverse)
library(dplyr)
library(lubridate)

fredr_set_key("ce46937f10cba232e2a383786dfac7ba")

# Define Series IDs you want to pull here
series_ids <- c(
  "UNRATE",   # Unemployment Rate
  "SP500",    # S&P 500 - Index
  "AHETPI",   # Avg Hourly Earnings - Total Private
  "UMCSENT"   # Univ of Michigan Consumer Sentiment
  )

# Pull data from the API and assign it to series ID for 4 series IDs
get_fred_data <- function(series_id) {
  fredr(
    series_id = series_id,
    observation_start = as.Date("2015-01-01"),
    observation_end = as.Date("2024-12-01")
  ) %>%
    mutate(series_id = series_id)
}

# Create a Data Frame
fred_data <- map_df(series_ids, get_fred_data)

# Split out the SP500 DATA and filter only to the last day of each month
sp500_data <- fred_data %>%
  filter(series_id == "SP500") %>%
  group_by(year = year(date), month = month(date)) %>%
  filter(date == max(date)) %>%
  ungroup()

# Subset of other monthly data
other_data <- fred_data %>%
  filter(series_id != "SP500")

# Combine SP500 and other monthly data sets together
fred_data_clean <- bind_rows(other_data, sp500_data)

write.csv(fred_data_clean,"FRED_data.csv")

# Build UI
ui <- fluidPage(
  titlePanel("Time Series Analysis Final Project"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("series", 
                  "Select Time Series dataset:",
                  choices = c("Unemployment Rate" = "UNRATE",
                              "S&P 500" = "SP500",
                              "Avg Hourly Earnings" = "AHETPI",
                              "Consumer Sentiment" = "UMCSENT"),   # <- Closing the c block
                  multiple = FALSE),   # <- Closing the selectInput block
      
      radioButtons("model", "Choose a Forecast Model:",
                   choices = c("ARIMA" = "arima",
                               "Random Walk" = "rw",
                               "Seasonal Naive" = "snaive"),  # <- Closing the c block
                   ),   # <- Closing the radioButtons block
      
      sliderInput("horizon", "Forecast Horizon (months):", min = 1, max = 12, 
                  value = 5)  # <- Closing the sliderInput block
    ),  # <- Closing the sidebarPanel block
    
    mainPanel(
      plotOutput("forecastPlot")
    )  # <- Closing the mainPanel block
    
  )  # <- Closing the sidebarLayout block
  
)  # <- Closing the fluidPage block

server <- function(input, output) {
  
  selected_series <- reactive({
    fred_data_clean %>%
      filter(series_id == input$series) %>%
      arrange(date)
  })
  
  output$forecastPlot <- renderPlot({
    df <- selected_series()
    print(df)
    
    # Convert to time series object
    start_year <- year(min(df$date))
    start_month <- month(min(df$date))
    ts_data <- ts(df$value, start = c(start_year, start_month), frequency = 12)
    
    fit <- switch(input$model,
                  "rw" = rwf(ts_data, drift = FALSE),
                  "arima" = auto.arima(ts_data),
                  "snaive" = snaive(ts_data))
    
    fc <- switch(input$model,
                 "rw" = rwf(ts_data, drift = FALSE, h = input$horizon),
                 "snaive" = snaive(ts_data, h = input$horizon),
                 "arima" = forecast(auto.arima(ts_data), h = input$horizon))
    
    autoplot(fc) +
      ggtitle(paste("Forecast of", input$series, "using", toupper(input$model))) +
      theme_classic()
  })
}

shinyApp(ui = ui, server = server)
