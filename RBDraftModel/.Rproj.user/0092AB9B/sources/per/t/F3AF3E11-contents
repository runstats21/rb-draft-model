# ui.R

# libraries
library(shiny)
library(tidyverse)
library(gbm)

## data ##
# full data
full_df <- read_csv("data/full_df.csv")
nfl_full_df <- read_csv("data/nfl_full_df.csv")

# modeling data
model_df <- read_csv("data/wp_model_df.csv")
nfl_model_df <- read_csv("data/nfl_model_df.csv")

# get info for select inputs
model_choices <- c("College Win Percentage","NFL Rushing Average")

wp_vars <- colnames(model_df)[-14]
nfl_vars <- colnames(nfl_model_df)[-14]

## UI ##
ui <- fluidPage(
  navbarPage(title = "RB NFL Draft App",
    tabPanel(title = "Draft Predictions",
             headerPanel(title = "RB NFL Draft Simulator"),
             
             sidebarPanel(
               
               numericInput(inputId = "draftyear",
                            label = "Choose Year:",
                            value = 2021, min = 1990, max = 2021),
               
               numericInput(inputId = "numplayers",
                            label = "Choose Number of Players to View:",
                            min = 1, max = 20, value = 10),
               
             ),
             mainPanel(
               plotOutput("draftplot")
             )
                 
             
             
             ),
    
    tabPanel(title = "Dependence Plots",
             
             headerPanel(title = "Effect of NCAA RB Statistics on Response of Interest"),
             
             
             sidebarLayout(
                sidebarPanel(
                  
                  selectInput(inputId = "model",
                              label = "Choose Response Variable:",
                              choices = model_choices),
                  
                  conditionalPanel(condition = "input.model == 'College Win Percentage'",
                                   
                                  selectInput(inputId = "wp.var",
                                              label = "Select Explanatory Variable:",
                                              choices = wp_vars,
                                              selected = "Rushing.TD",
                                              selectize = TRUE)
                                   
                                   ),
                  
                  conditionalPanel(condition = "input.model == 'NFL Rushing Average'",
                                   
                                   selectInput(inputId = "nfl.var",
                                               label = "Select Explanatory Variable:",
                                               choices = nfl_vars,
                                               selected = "Rushing.TD",
                                               selectize = TRUE)
                                   
                                   ),
                  
                  
                ),
            mainPanel(
          
              plotOutput("pdpplot")
          
            )
          ) # end sidebar layout tab
      
    ) # end PDP plots tab
    
  ) # end tabsetPanel
  
  
) # end fluidPage (UI)