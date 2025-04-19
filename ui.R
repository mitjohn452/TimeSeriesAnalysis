library(tidyverse)
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)
 
 server <- function(input, output) { }
 
 shinyApp(ui, server)
 
 header <- dashboardHeader(title = "Demo Predictor")
 
 sidebar <- dashboardSidebar(
   sidebarMenu(
     menuItem("Predict!", tabName = "model", icon = icon("bar-chart-o"),
              selectInput("cyl",
                          "Number of Cylinders",
                          c(4,6,8)),
              numericInput("disp",
                           "Displacement",
                           100),
              numericInput("hp",
                           "Horsepower",
                           100),
              numericInput("drat",
                           "Rear axle ratio",
                           3),
              numericInput("wt",
                           "Weight (1000 lbs)",
                           3),
              numericInput("qsec",
                           "Quarter mile time",
                           20),
              selectInput("vs",
                          "Engine",
                          c("V-shaped", "straight")),
              selectInput("am",
                          "Transmission",
                          c("Automatic", "Manual")),
              selectInput("gear",
                          "Number of gears",
                          c(3,4,5)),
              selectInput("carb",
                          "Number of carburetors",
                          c(2,3,4,6,8)),
              div(style="display: inline-block;vertical-align:top; width: 100px;",
                  actionButton("go", "Predict!")),
              div(style="display: inline-block;vertical-align:top; width: 100px;",
                  actionButton("reset", "Clear", style='padding:6px;width:80px'))
     )
   )
 )
 
 body <- dashboardBody()
 
 ui <- dashboardPage(header, 
                     sidebar, 
                     body)
 
 body <- dashboardBody(
   h2("The predicted miles per gallon is: "),
   h3(verbatimTextOutput("pred", placeholder = T)),
   tags$head(tags$style("#pred{color: black;
                                   font-size: 20px;
                                   font-family: Source Sans Pro
                                   }")),
   fluidRow(
     infoBox(
       "What", "is this model for?", icon = icon("line-chart"),
       width = 4
     ),
   infoBox(
     "Who", "is this model for?", icon = icon("user-friends"),
     width = 4
   ),
   infoBox(
     "Where", "can I find out more?", icon = icon("book-open"),
     width = 4
   )),
 fluidRow(
   column(align = "center",
          "A Medium article demonstration",
          width = 4
   ),
   column(
     align = "center",
     "Everyone!",
     width = 4
   ),
   column(
     align = "center",
     "To read more visit: ",
     width = 4
   ))
 )
 
 ui <- dashboardPage(header, 
                     sidebar, 
                     body, 
                     shinyjs::useShinyjs()
 )
 