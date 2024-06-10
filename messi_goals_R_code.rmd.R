# Daniella Prado 
# Messi Goals 
#June 10, 2024 
# data from: https://www.kaggle.com/datasets/azminetoushikwasi/-lionel-messi-all-club-goals 

# Read in data ------------------------------------------------------------
data <- read.csv("/Users/Daniella/Desktop/github/messi_goals/data.csv")

# Data cleaning/understanding -----------------------------------------------------------
# get rid of any empty columns 
clean_data <- na.omit(data)

# Get data types for messi scores 
score_type <- unique(data$Type) 
numberOfCategories <- length(score_type)
print(score_type)
data <- data %>%
  filter(Type != "")


# See what clubs messi played for 
clubs <- unique(data$Club) 
numberofClubs <- length(clubs)
print(clubs)

# Get information on competitions played 
competitions <- unique(data$Competition) 
numberofCompetitions <- length(competitions)
print(competitions)

# Get information on seasons 
seasons <- unique(data$Season) 
numberofSeasons <- length(seasons)
print(seasons)

# Standardize the way seasons are displayed on dataset 
library(dplyr)
data <- data %>%
  mutate(Season = if_else(Season == "11-Dec", "12/11", Season))
data <- data %>%
  mutate(Season = if_else(Season == "Dec-13", "12/13", Season))

# Get information on matchday 
matchdays <- unique(data$Matchday) 
numberofmatchday <- length(matchdays)
print(matchdays)

# Standardize the formatting for dates  
data <- data %>%
  mutate(Date = str_replace_all(Date, "-", "/"))


# Create column with a club's goals 
data <- data %>%
  mutate(
    `club's goals` = if_else(Venue == "H", as.numeric(str_extract(Result, "^[0-9]+")), as.numeric(str_extract(Result, "(?<=:)[0-9]+"))),
    `opponent's goals` = if_else(Venue == "H", as.numeric(str_extract(Result, "(?<=:)[0-9]+")), as.numeric(str_extract(Result, "^[0-9]+")))
  )

# Create new columns for club's goals and opponent's goals, as well as an additional column telling us if Messi's club won or not 
data <- data %>%
  mutate(
    clubs_goals = if_else(Venue == "H", as.numeric(str_extract(Result, "^[0-9]+")), as.numeric(str_extract(Result, "(?<=:)[0-9]+"))),
    opponent_goals = if_else(Venue == "H", as.numeric(str_extract(Result, "(?<=:)[0-9]+")), as.numeric(str_extract(Result, "^[0-9]+"))),
    club_wins = if_else(`club's goals` > `opponent's goals`, 1, 0)
  )


# Statistics on the positions Messi plays 
positions <- unique(data$Playing_Position) 
numberofPositions <- length(positions)
print("Positions Messi has played")
print(positions)

# Statistics on Messi's opponents 
opponents <- unique(data$Opponent) 
numberofOpponents <- length(opponents)
print(opponents)

# Statisics on Messi's assists 
assists <- unique(data$Goal_assist) 
numberofAssists <- length(assists)
print(assists)

# if the goal did not have someone assist, write none 
data$Goal_assist <- ifelse(data$Goal_assist == "", "None", data$Goal_assist)

# Extracting data ---------------------------------------------------------

# Get data on Messi's wins and losses
total_wins <- sum(as.numeric(data$club_wins), na.rm = TRUE)
sprintf("Since the season of 04/05 to 22/23, Messi's teams won %d", total_wins)
total_losses <- length(data$club_wins) - total_wins
sprintf("Since the season of 04/05 to 22/23, Messi's teams lost %d", total_losses)



