# Redevelopment work
# last modified: 7/24/2023
# written by Tyler Ward



# Redevelopment goals:
# 0: recreate/produce data import (i.e., webscraping, cleaning, rowbinding)
# libraries
library(tidyverse) # for pipes, purrr for mapping vector of urls to scraping function
library(rvest) # for webscraping
# library(gbm) # for "Gradient Boosting Machine" models

### Data import ###

## Functions ##

# functions that should help with error time outs
library(magrittr)
library(tibble)
library(dplyr)
library(purrr)

#' Retrieve the open connection(s) pointing to URI
#'
#' @param uri Character: path or URL the connection points to.
#'
#' @return A list of connection objects.
#'
#' @importFrom magrittr %>%
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr filter pull
#' @importFrom purrr map
#' @noRd
get_connections <- function(uri){

    showConnections(all = TRUE) %>%
    as.data.frame %>%
    rownames_to_column('con_id') %>%
    filter(description == uri) %>%
    pull(con_id) %>%
    as.integer %>%
    map(getConnection)

}


#' Closes the open connection(s) pointing to URI
#'
#' @param uri Character: path or URL the connection points to.
#'
#' @return Invisible `NULL`.
#'
#' @importFrom magrittr %>%
#' @importFrom purrr walk
#' @noRd
close_connection <- function(uri){

    uri %>%
    get_connections %>%
    walk(close)

    invisible(NULL)

}

# function to scrape sportsreference.com/cfb rushing stats page
import_position_data <- function(url) {
  
  # create table for given position
  pos_tbl <- read_html(url) %>%
  html_nodes("table") %>%
  html_table()
  
  colnames(pos_tbl) = paste0(colnames(pos_tbl), ".", pos_tbl[1,]) # add periods after position definition
  
  colnames(pos_tbl)[1:5] = gsub("\\.", "", colnames(pos_tbl[1:5])) # remove periods from first few columns
  
  pos_tbl = pos_tbl[-1,] # remove unneeded header row
  
  pos_tbl = pos_tbl[!grepl('Rk', pos_tbl$Rk), ]  # remove other unneeded repeated header rows
  
  pos_tbl[, -(1:4)]= sapply(pos_tbl[, -(1:4)], as.numeric) # ensure rushing stats are interpretted as numeric variables
  
  pos_tbl_out <- pos_tbl %>% 
    mutate(Year = as.integer(str_extract(url, "\\d{4}"))) %>% 
    select(Year, everything()) %>% 
    select(-Rk) %>% # remove Rk variable
    mutate(Player = str_replace_all(Player, "[*+]", "")) # remove stars and other symbols from data values
  
  return(pos_tbl_out)
}

# function to get wins from season standings table
get_wins_data <- function(url) {
  
  standing_tbl <- read_html(url) %>% 
  html_element("table") %>% 
  html_table()

  colnames(standing_tbl) = paste0(colnames(standing_tbl), ".", standing_tbl[1,])
 
  colnames(standing_tbl)[1:3] = gsub("\\.", "", colnames(standing_tbl[1:3]))  # remove periods from first few columns

  # remove unneeded rows
  standing_tbl = standing_tbl[-1,] 
  standing_tbl_clean1 = standing_tbl[!grepl('Rk', standing_tbl$Rk), ]

  standing_tbl_clean2 = standing_tbl_clean1[!grepl('Overall', standing_tbl_clean1$Overall.W),]   # remove unneed repeated stat specification rows
  
  standing_tbl_out = standing_tbl_clean2 %>% 
    select(-`.Notes`) %>% 
    mutate(Year = as.integer(str_extract(url, "\\d{4}"))) # get Year for joining

  standing_tbl_out[, -(1:3)]= sapply(standing_tbl_out[, -(1:3)], as.numeric)   # make continuous variables numeric
  
  return(standing_tbl_out)
}

# function for scraping NFL reference page
import_nfl_position_data <- function(url) {
  # create table for given position
  pos_tbl <- read_html(url) %>%
  html_node("table") %>%
  html_table()
  
  colnames(pos_tbl) = paste0(colnames(pos_tbl), ".", pos_tbl[1,]) # add periods after position definition
  
  colnames(pos_tbl)[1:5] = gsub("\\.", "", colnames(pos_tbl[1:5])) # remove periods from first few columns
 
  pos_tbl = pos_tbl[-1,]  # remove unneeded row
  
  # remove other unneeded repeated header rows
  pos_tbl = pos_tbl[!grepl('Rk', pos_tbl$Rk), ]
  
  pos_tbl[, -(1:5)]= sapply(pos_tbl[, -(1:5)], as.numeric)
  
  pos_tbl_out <- pos_tbl %>% 
    mutate(Year = as.integer(str_extract(url, "\\d{4}"))) %>% 
    select(-Rk) %>% # remove rank column
    select(Year, Player, everything())  # select Year and Player first
    
  return(pos_tbl_out)
}

## URLS ##
showConnections()

# years to get
years <- c(2005:2022)
nfl_years <- c(2006:2022)

# urls of all rb stats (up to 400 per year)
rush_urls <- str_c("https://www.sports-reference.com/cfb/years/", years, "-rushing.html")  

# urls for all teams for all rbs
standing_urls <- str_c("https://www.sports-reference.com/cfb/years/", years, "-standings.html")

# get urls of all rb stats (up to 400 per year)
nfl_rush_urls <- str_c("https://www.pro-football-reference.com/years/", nfl_years, "/rushing.htm")

# test functions #
# test urls
rush_urls[1]
nfl_rush_urls[length(nfl_rush_urls)]
import_position_data(rush_urls[3]) # TOFIX: this throws error, look into this
import_nfl_position_data(nfl_rush_urls[length(nfl_rush_urls)])
get_wins_data(standing_urls[1])

close_connection(rush_urls[3])

# examine AP Polls vars
get_wins_data(standing_urls[1])$`Polls.AP Rank` %>% summary() # 1-25 or NaN
# so, could easily create a flag of whether or not this field is NaN


## DFS ##
# create rushing and wins dfs for all specified years (i.e., all urls stored in above vectors)
# TOFIX: sports reference does not allow more than 20 requests per minute ::: add sys.sleep(60) between function calls
rush_df <- map_dfr(rush_urls, import_position_data)
Sys.sleep(60)
wins_df <- map_dfr(standing_urls, get_wins_data)
Sys.sleep(60)
nfl_df <- map_dfr(nfl_rush_urls, import_nfl_position_data)

### Clean data ###

rush_clean <- rush_df %>% 
  # normalize Yard and Attempt statistics (Yards Gained/Number of Games Played)
  mutate(Rushing.Yds.PG = Rushing.Yds/G,
         Receiving.Yds.PG = Receiving.Yds/G,
         Scrimmage.Yds.PG = Scrimmage.Yds/G,
         Rushing.Att.PG = Rushing.Att/G) %>% 
  select(-c(Rushing.Yds,Scrimmage.Yds,Receiving.Yds,Rushing.Att, # remove original variables that were normalized and replaced
            Scrimmage.Plays)) # and take out "plays" variable, which is just attempts plus receptions
  
wins_clean <- wins_df %>% 
  select(School,Year,`Points Per Game.Off`,SRS.SOS,Overall.Pct) %>% 
  rename(PPG.Off = `Points Per Game.Off`) %>% 
  mutate(School = str_replace_all(School, "BYU", "Brigham Young")) %>% 
  mutate(School = str_replace_all(School, "Texas Christian", "TCU")) %>% 
  mutate(School = str_replace_all(School, "Nevada-Las Vegas", "UNLV"))
  
# join by school and year
full_df <- rush_clean %>% 
  left_join(wins_clean, by = c("School", "Year"))
  
# set NA values to zeros
full_df[is.na(full_df)] <- 0

# holdout 2022 rbs
# test_df = 

# write full df to a csv file (for use in Shiny App)
write_csv(full_df, file = "./full_df_redev.csv")

#unique(nfl_df$Pos) # check Position names

nfl_rb_df <- nfl_df %>% 
  filter(Pos %in% c("RB", "rb", "/rb")) %>% 
  # remove Games from df
  select(-c(Games.G,Games.GS)) %>% 
  # remove stars and other symbols from dataset
  mutate(Player = str_replace_all(Player, "[*+]", ""))

# get only needed columns
rb_join_df <- nfl_rb_df %>% 
  select(Year,Player,`Rushing.Y/A`) %>% 
  group_by(Player) %>% 
  mutate(NFL.Rushing.Avg = mean(`Rushing.Y/A`),
         NFL.Rushing.Avg.Max = max(`Rushing.Y/A`)) %>% 
  select(-`Rushing.Y/A`)

# join cfb and nfl datasets
nfl_full_df <- full_df %>% 
  inner_join(rb_join_df, by = "Player") %>% # this will only get RBs who have made it to the nfl
  select(-Year.y) %>% 
  unique()



# 1. explore different modeling options (i.e., different ML models to complete current task)
## note: try using caret to aid in this task
library(gbm) # for "Gradient Boosting Machine" models
library(ranger)
library(caret)


# 2. explore variable importance, variable selection, and data wrangling options
## idea(s): 
# add "played postseason" flag
# add AP polls var; could be flag of is not null? (i.e., in top 25)

# 3. Redefine response variables used to rank players in mock draft
## e.g., use AP readiness score, or aggregate of different metrics in ranking
## currently using avg. of Z-score for Overall_Win_perc and Z-score for NFL-rushing-avg