library(tidyverse)
library(shiny)
library(shinydashboard)

server <- function(input, output, session) { 
  
  #reset
  observeEvent(input$reset, {
    updateSelectInput(session, 'cyl')
    updateNumericInput(session, 'disp', value = 100)
    updateNumericInput(session, 'hp', value = 100)
    updateNumericInput(session, "drat", value = 3)
    updateNumericInput(session, "wt", value = 3)
    updateNumericInput(session, "qsec", value = 20)
    updateSelectInput(session, 'vs')
    updateSelectInput(session, 'am')
    updateSelectInput(session, 'gear')
    updateSelectInput(session, 'carb')
  })
  
  fit <- lm(mpg ~ ., data = mtcars)
  
  pred <- eventReactive(input$go, {
    
    newdata <- tibble(cyl = input$cyl,
                      disp = input$disp,
                      hp = input$hp,
                      drat = input$drat,
                      wt = input$wt,
                      qsec = input$qsec,
                      vs = input$vs,
                      am = input$am,
                      gear = input$gear,
                      carb = input$carb) %>%
      mutate(vs = case_when(
        vs == "V-shaped" ~ 0,
        vs == "Straight" ~ 1
      ),
      am = case_when(
        am == "Automatic" ~ 0,
        am == "Manual" ~ 1
      )) %>%
      mutate_all(as.numeric)
    
    pred <- predict(fit, newdata = newdata)
    pred <- round(pred, 2)
    paste0(pred)
  })
  
  output$pred <- renderText(pred())
}