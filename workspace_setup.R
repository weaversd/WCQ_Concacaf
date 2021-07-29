#packages
library(tidyverse)
library(rvest)
library(readxl)
library(lubridate)
library(reshape2)
library(data.table)
library(dplyr)
library(formattable)
library(tidyr)
library(sparkline)

#score file
score_file <- "scores.xlsx"

#Variables
teams <- list("Canada", "Costa Rica", "El Salvador", "Honduras",
              "Jamaica", "Mexico", "Panama", "United States")
games_n <- 14

sim_seasons_n <- 1000


#scores
raw_scores_import <- read_excel(score_file)
raw_scores_import$Date <- dmy(raw_scores_import$Date)
scores <- raw_scores_import
scores$Winner <- NA
scores$Loser <- NA
scores$Draw <- NA

#winners and losers
for (i in 1:nrow(scores)) {
  if(!is.na(scores$H_score[i]) & (scores$H_score[i] == scores$A_score[i])){
    scores$Winner[i] <- "Draw"
    scores$Loser[i] <- "Draw"
    scores$Draw[i] <- T
  } else if (!is.na(scores$H_score[i]) & (scores$H_score[i] != scores$A_score[i])){
    scores$Draw[i] <- F
    if (scores$H_score[i] > scores$A_score[i]){
      scores$Winner[i] <- scores$Home[i]
      scores$Loser[i] <- scores$Away[i]
    } else {
      scores$Winner[i] <- scores$Away[i]
      scores$Loser[i] <- scores$Home[i]
    }
  }
}

#subset_by_team
subset_by_team <- function(team, df = scores) {
  team_df <- df[(df$Home == team) | (df$Away == team),]
  return(team_df)
}

#create table
create_table <- function(df = scores) {
  Team <- rep("", 8)
  GP <- rep(0, 8)
  W <- rep(0, 8)
  L <- rep(0, 8)
  D <- rep(0, 8)
  Pts <- rep(0, 8)
  GF <- rep(0, 8)
  GA <- rep(0, 8)
  GD <- rep(0, 8)
  counter <- 1
  
  for (team in teams) {
    temp_team <- subset_by_team(team, df = df)
    temp_team_played <- temp_team[!is.na(temp_team$H_score),]
    if (nrow(temp_team_played) == 0){
      games_played <- 0
      temp_draws <- 0
      temp_wins <- 0
      temp_losses <- 0
      temp_losses <- sum(temp_team_played$Loser == team)
      temp_points <- (3*temp_wins) + (temp_draws)
      
      temp_goals_for <- 0
      temp_goals_against <- 0
      temp_goal_diff <- temp_goals_for - temp_goals_against
    } else {
      games_played <- nrow(temp_team_played)
      temp_draws <- sum(temp_team_played$Draw)
      temp_wins <- sum(temp_team_played$Winner == team)
      temp_losses <- sum(temp_team_played$Loser == team)
      temp_points <- (3*temp_wins) + (temp_draws)
      
      temp_goals_for <- 0
      temp_goals_against <- 0
      for (i in 1:nrow(temp_team_played)) {
        if (temp_team_played$Home[i] == team) {
          temp_goals_for <- temp_goals_for + temp_team_played$H_score[i]
          temp_goals_against <- temp_goals_against + temp_team_played$A_score[i]
        } else {
          temp_goals_for <- temp_goals_for + temp_team_played$A_score[i]
          temp_goals_against <- temp_goals_against + temp_team_played$H_score[i]
        }
      }
      temp_goal_diff <- temp_goals_for - temp_goals_against
    }
    Team[counter] <- team
    GP[counter] <- games_played
    W[counter] <- temp_wins
    L[counter] <- temp_losses
    D[counter] <- temp_draws
    Pts[counter] <- temp_points
    GF[counter] <- temp_goals_for
    GA[counter] <- temp_goals_against
    GD[counter] <- temp_goal_diff
    
    counter <- counter + 1
  }
  table <- data.frame(Team, GP, W, L, D, Pts, GF, GA, GD)
  ordered_table <- table[order(-Pts, -GD, -GF),]
  ordered_table$Pos <- seq.int(nrow(ordered_table))
  return(ordered_table)
}

current_table <- create_table()

formattable(current_table, row.names = F,
            align = c("l", "c", "c", "c", "c", "c", "c", "c", "c", "c"),
            list(`Pts`= color_bar("lightblue"),
                 `GD` = color_tile("pink", "lightgreen"),
                 `GF` = color_bar("lightgreen"),
                 `GA` = color_bar("pink"),
                 `W` = color_bar("lightgreen"),
                 `L` = color_bar("pink"),
                 `D` = color_bar("khaki"))
            )
