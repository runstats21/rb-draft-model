# Webscrape of NFL RBs Drafted by Year
# from http://www.drafthistory.com/index.php/positions/rb

# libraries
library(tidyverse)
library(rvest)

# scrape ####
# save url
url = "http://www.drafthistory.com/index.php/positions/rb"

data_tbl <- read_html(url) %>% # read html elements
  html_element("table") %>% # select table element
  html_table() # convert to R data frame

# clean ####
colnames(data_tbl) = data_tbl[2,] # define header

# remove excess rows
data_tbl_clean = data_tbl[-c(1:2),] %>% mutate(Year = as.numeric(Year)) %>%
  # forward fill in year values
  fill(Year)

write_csv(data_tbl_clean, "./R_development_files/rbsDraftedThru2023.csv")

