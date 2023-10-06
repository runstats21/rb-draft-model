# server.R

# libraries (make sure these libraries are installed before running)
library(tidyverse)
library(gbm)
library(shiny)

# set seed (for reproducability)
set.seed(1)

# shiny selections
# get info for select inputs
model_choices <- c("College Win Percentage","NFL Rushing Average")

## data ##
# full data
full_df <- read_csv("data/full_df.csv")
nfl_full_df <- read_csv("data/nfl_full_df.csv")

# model data
model_df <- read_csv("data/wp_model_df.csv")
nfl_model_df <- read_csv("data/nfl_model_df.csv")
nonmodel_cols = c(1,2,3,4,5)

## Fit BEST MODELS ##
# Win Percentage (WP) model
cfbX = model.matrix(Overall.Pct ~ ., data=model_df)[,-c(1)] # model matrix

gbm_mod_wp = gbm::gbm(model_df$Overall.Pct~.,
                      data=data.frame(cfbX),
                      n.trees=300)

# NFL Rushing Avg (NFL) model
nflX = model.matrix(NFL.Rushing.Avg ~., data=nfl_model_df)[,-c(1)]

gbm_mod_nfl <- gbm::gbm(nfl_model_df$NFL.Rushing.Avg~.,
                        data = data.frame(nflX),
                        n.trees = 500)


server <- function(input, output) {
  

  observeEvent(input$model, {
    
    # PDP plots based on which model is chosen
    if(input$model == model_choices[1]) {
      
      output$pdpplot <- renderPlot({
        # pdp plot from nfl mod
        plot(gbm_mod_wp, i.var = input$wp.var,
             ylab = "Win Percentage",
             main = "Partial Dependence Plot for Predicted College Win Percentage",
             cex = 5)
      })
    } else {
      
      output$pdpplot <- renderPlot({
        # pdp plot from nfl mod
        plot(gbm_mod_nfl, i.var = input$nfl.var,
             ylab = "NFL Rushing Yards Per Carry",
             main = "Partial Dependence Plot for Predicted NFL Rushing Avg",
             cex = 5)
      })
      
    }
    
    
  })
  
  
  ##### Draft Plot #####
  
  # reactive dataset
  # reactive to chosen draft year
  draft_data <- eventReactive(input$draftyear, {
    
    year_df <- full_df %>% 
      # input = year of draft (e.g. 2022 in 2021-2022 season)
      # dataset `Year` = year of college season (e.g. 2021 in 2021-2022 season)
      filter(Year == (input$draftyear-1)) %>% 
      select(-c(Overall.Pct,PPG.Off))
    
    # predict wp for 2021
    wp.preds <- predict(gbm_mod_wp, year_df[, -nonmodel_cols], type = "response")
    
    # cbind the predictions back to the df
    full_wp_preds <- cbind(year_df, wp.preds)
    
    # See Player name and prediction
    # order from highest to lowest
    pp.wp <- full_wp_preds %>% 
      select(Player, wp.preds) %>% 
      arrange(desc(wp.preds))
    
    # calculate and add z-score column
    pp.wp <- pp.wp %>% 
      mutate(wp.z = (wp.preds - mean(wp.preds)) / sd(wp.preds) )
    
    # get predictions from NFL rushing Avg Model
    
    # predict nfl avg for 2021
    nfl.preds <- predict(gbm_mod_nfl, year_df[, -nonmodel_cols], type = "response")

    #  cbind the predictions back to the df
    full_nfl_preds <- cbind(year_df, nfl.preds)

    # View predictions in order
    pp.nfl <- full_nfl_preds %>% 
      select(Player, nfl.preds) %>% 
      arrange(desc(nfl.preds))
    
    # calculate z-score column
    pp.nfl <- pp.nfl %>% 
      mutate(nfl.z = (nfl.preds - mean(nfl.preds)) / sd(nfl.preds) )
    #pp.wp.df <- readRDS("pp.wp.Rdata") # preds from WP model
    
    ## Aggregation of both Models ##
    # join preds from both models together
    full_preds <- pp.wp %>% inner_join(pp.nfl, by = "Player") 
    
    # clean preds
    # calculate mean z-score
    draft <- full_preds %>% 
      mutate(Z = wp.z + nfl.z / 2) %>% 
      select(Player, Z) %>% 
      arrange(desc(Z)) 
    
    return(draft)
      
    
  })
  
  # Plot of Draft
  # Reactive to Number of Players Chosen To View
  
  output$draftplot <- renderPlot({
    
    ggplot(data = draft_data() %>% head(input$numplayers),
           aes(x = Z,
               y = reorder(Player,Z) )) +
      geom_col() +
      labs(x = "Aggregated Z-score",
          y = "Player",
          title = paste0(input$draftyear," Mock Running Back Draft (Top ", input$numplayers,")"),
          subtitle = "Based on Z-score of predicted NCAA Win Percentage and NFL Yards Per Carry") +
      geom_text(mapping = aes(label = round(Z,2)),
                nudge_x = 0.1,
                size = 4) +
      theme_bw()+
      theme(axis.text = element_text(size = 13),
            axis.title = element_text(size = 13),
            title = element_text(size = 14))
  })

  
}