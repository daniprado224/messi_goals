# Daniella Prado 
# Messi Goals 
#June 10, 2024 
# data from: https://www.kaggle.com/datasets/azminetoushikwasi/-lionel-messi-all-club-goals 

# Read in data ------------------------------------------------------------
data <- read.csv("/Users/Daniella/Desktop/github/messi_goals/data.csv")


# Data cleaning/understanding -----------------------------------------------------------
# get rid of any empty columns 
clean_data <- na.omit(data)

# get data types for messi scores 
score_type <- unique(data$Type) 
numberOfCategories <- length(score_type)
print(score_type)

# see what clubs messi played for 
clubs <- unique(data$Club) 
numberofClubs <- length(clubs)
print(clubs)

# get information on competitions played 
competitions <- unique(data$Competition) 
numberofCompetitions <- length(competitions)
print(competitions)

# get information on seasons 
seasons <- unique(data$Season) 
numberofSeasons <- length(seasons)
print(seasons)

# standardize the way seasons are displayed on dataset 
library(dplyr)
data <- data %>%
  mutate(Season = if_else(Season == "11-Dec", "12/11", Season))
data <- data %>%
  mutate(Season = if_else(Season == "Dec-13", "12/13", Season))

# get information on matchday 
matchdays <- unique(data$Matchday) 
numberofmatchday <- length(matchdays)
print(matchdays)

# standardize the formatting for dates  
data <- data %>%
  mutate(Date = str_replace_all(Date, "-", "/"))

# Extracting data ---------------------------------------------------------






