# Daniella Prado 
# Messi Goals 
# June  2024 
# data from: https://www.kaggle.com/datasets/azminetoushikwasi/-lionel-messi-all-club-goals  

# Read in data ------------------------------------------------------------
data <- read.csv("/Users/Daniella/Desktop/github/messi_goals/data.csv")

# Data cleaning/understanding -----------------------------------------------------------

# Get data types for messi scores 
score_type <- unique(data$Type) 
numberOfCategories <- length(score_type)
print(score_type)

# Clean column from any empty row 
data <- data %>%
  filter(Type != "")

# Get list of clubs 
clubs <- unique(data$Club) 
numberofClubs <- length(clubs)
print(clubs)

# Get information on competitions played 
competitions <- unique(data$Competition) 
numberofCompetitions <- length(competitions)
print(competitions)

# Standardize competition names 
# Change to Champions League 
data <- data %>%
  mutate(Competition = str_replace_all(Competition, "Troph�e des Champions", "Champions League"))
data <- data %>%
  mutate(Competition = str_replace_all(Competition, "Trophée des Champions", "Champions League"))
data <- data %>%
  mutate(Competition = str_replace_all(Competition, "UEFA Champions League", "Champions League"))

# Correct grammar 
data <- data %>%
  mutate(Competition = str_replace_all(Competition, "LaLiga", "La Liga"))

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

# Create new columns for club's and opponent's goals, as well as an additional column telling us if Messi's club won or not 
data <- data %>%
  mutate(
    clubs_goals = if_else(Venue == "H", as.numeric(str_extract(Result, "^[0-9]+")), as.numeric(str_extract(Result, "(?<=:)[0-9]+"))),
    opponent_goals = if_else(Venue == "H", as.numeric(str_extract(Result, "(?<=:)[0-9]+")), as.numeric(str_extract(Result, "^[0-9]+"))),
    club_wins = if_else(clubs_goals > opponent_goals, 1, 0)
  )

# Statistics on the positions Messi plays 
positions <- unique(data$Playing_Position) 
numberofPositions <- length(positions)
print(positions)

# Statistics on Messi's opponents 
opponents <- unique(data$Opponent) 
numberofOpponents <- length(opponents)
print(opponents)

# Statisics on Messi's assists (who assisted him)
assists <- unique(data$Goal_assist) 
numberofAssists <- length(assists)
print(assists)

# Clean data from outlier 
data <- data %>%
  filter(Goal_assist != "\t")

# If the goal did not have someone assist, write none 
data$Goal_assist <- ifelse(data$Goal_assist == "", "None", data$Goal_assist)

# Extracting data ---------------------------------------------------------

# Get data on Messi's wins and losses
total_wins <- sum(as.numeric(data$club_wins), na.rm = TRUE)
sprintf("Since the season of 04/05 to 22/23, Messi's teams won %d", total_wins)
total_losses <- length(data$club_wins) - total_wins
sprintf("Since the season of 04/05 to 22/23, Messi's teams lost %d", total_losses)

# Define the names of the values
names <- c("Wins", "Losses")
values <- c(total_wins, total_losses)
# Create the bar plot
barplot(values, names.arg = names, col = "pink", main = "Wins vs. Losses Total", ylab = "Stats")

# Add value labels on top of the bars
text(x = 1:length(data), y = values, label = values, pos = 3, cex = 0.8, col = "red")

# Divide these wins and losses by the competition played 
total_liga <- subset(data, Competition == "La Liga")
total_champions <- subset(data, Competition == "Champions League")
total_copa_rey <- subset(data, Competition == "Copa del Rey")
total_supercopa <- subset(data, Competition == "Supercopa")
total_world_cup <- subset(data, Competition == "FIFA Club World Cup")
total_uefa_supercup <- subset(data, Competition == "UEFA Super Cup")
total_ligue1 <- subset(data, Competition == "Ligue 1")

# Define the names of the values
total_wins_liga <- sum(as.numeric(total_liga$club_wins), na.rm = TRUE)
total_losses_liga <- length(total_liga$club_wins) - total_wins_liga
names <- c("Wins", "Losses")
values <- c(total_wins_liga, total_losses_liga)
# Create the bar plot
barplot(values, names.arg = names, col = "blue", main = "Wins vs. Losses Liga", ylab = "Stats", ylim=c(0,450))

# Add value labels on top of the bars
text(x = 1:length(total_liga), y = values, label = values, pos = 3, cex = 0.8, col = "red")

# Define the names of the values
total_wins_champions <- sum(as.numeric(total_champions$club_wins), na.rm = TRUE)
total_losses_champions <- length(total_champions$club_wins) - total_wins_champions
names <- c("Wins", "Losses")
values <- c(total_wins_champions, total_losses_champions)
# Create the bar plot
barplot(values, names.arg = names, col = "green", main = "Wins vs. Losses Champions", ylab = "Stats")

# Add value labels on top of the bars
text(x = 1:length(data), y = values, label = values, pos = 3, cex = 0.8, col = "red")

# Define the names of the values
total_wins_copa_rey <- sum(as.numeric(total_copa_rey$club_wins), na.rm = TRUE)
total_losses_copa_rey <- length(total_copa_rey$club_wins) - total_wins_copa_rey
names <- c("Wins", "Losses")
values <- c(total_wins_copa_rey, total_losses_copa_rey)
# Create the bar plot
barplot(values, names.arg = names, col = "yellow", main = "Wins vs. Losses Copa Rey", ylab = "Stats")

# Add value labels on top of the bars
text(x = 1:length(data), y = values, label = values, pos = 3, cex = 0.8, col = "red")

# Define the names of the values
total_wins_supercopa <- sum(as.numeric(total_supercopa$club_wins), na.rm = TRUE)
total_losses_supercopa <- length(total_supercopa$club_wins) - total_wins_supercopa
names <- c("Wins", "Losses")
print(length(total_supercopa))
values <- c(total_wins_supercopa, total_losses_supercopa)
# Create the bar plot
barplot(values, names.arg = names, col = "orange", main = "Wins vs. Losses Supercopa", ylab = "Stats")

# Add value labels on top of the bars
text(x = 1:length(data), y = length(total_supercopa), label = values, pos = 3, cex = 0.8, col = "red")

# Define the names of the values
total_wins_world_cup <- sum(as.numeric(total_world_cup$club_wins), na.rm = TRUE)
total_losses_world_cup <- length(total_world_cup$club_wins) - total_wins_world_cup
names <- c("Wins", "Losses")
values <- c(total_wins_world_cup, total_losses_world_cup)
# Create the bar plot
barplot(values, names.arg = names, col = "grey", main = "Wins vs. Losses World Cup", ylab = "Stats")

# Add value labels on top of the bars
text(x = 1:length(data), y = values, label = values, pos = 3, cex = 0.8, col = "red")

# Define the names of the values
total_wins_uefa_supercup<- sum(as.numeric(total_uefa_supercup$club_wins), na.rm = TRUE)
total_losses_uefa_supercup<- length(total_uefa_supercup$club_wins) - total_wins_uefa_supercup
names <- c("Wins", "Losses")
values <- c(total_wins_uefa_supercup, total_losses_uefa_supercup)
# Create the bar plot
barplot(values, names.arg = names, col = "purple", main = "Wins vs. Losses Supercup", ylab = "Stats")

# Add value labels on top of the bars
text(x = 1:length(data), y = values, label = values, pos = 3, cex = 0.8, col = "red")

# Define the names of the values
total_wins_ligue1 <- sum(as.numeric(total_ligue1$club_wins), na.rm = TRUE)
total_losses_ligue1 <- length(total_ligue1$club_wins) - total_wins_ligue1
names <- c("Wins", "Losses")
values <- c(total_wins_ligue1, total_losses_ligue1)
# Create the bar plot
barplot(values, names.arg = names, col = "cyan", main = "Wins vs. Losses Ligue 1", ylab = "Stats")

# Add value labels on top of the bars
text(x = 1:length(data), y = values, label = values, pos = 3, cex = 0.8, col = "red")

