# libraries
library(tidyverse) # for pipes, purrr for mapping vector of urls to scraping function
library(rvest) # for scrapings
library(gbm) # for "Gradient Boosting Machine" models

### Data import ###

## Functions ##

# function to scrape sportsreference.com/cfb rushing stats page
import_position_data <- function(url) {
  # create table for given position
  pos_tbl <- read_html(url) %>%
  html_node("table") %>%
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

# years to get
years <- c(1990:2021)
nfl_years <- c(1991:2021)

# urls of all rb stats (up to 400 per year)
rush_urls <- str_c("https://www.sports-reference.com/cfb/years/", years, "-rushing.html")  

# urls for all teams for all rbs
standing_urls <- str_c("https://www.sports-reference.com/cfb/years/", years, "-standings.html")

# get urls of all rb stats (up to 400 per year)
nfl_rush_urls <- str_c("https://www.pro-football-reference.com/years/", nfl_years, "/rushing.htm")  

## DFS ##
# create rushing and wins dfs for all specified years (i.e., all urls stored in above vectors)
rush_df <- map_dfr(rush_urls, import_position_data)
wins_df <- map_dfr(standing_urls, get_wins_data)
nfl_df <- map_dfr(rush_urls, import_nfl_position_data)


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

# write full df to a csv file (for use in Shiny App)
write_csv(full_df, file = "./full_df.csv")

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
  inner_join(rb_join_df, by = "Player") %>% 
  select(-Year.y) %>% 
  unique()

