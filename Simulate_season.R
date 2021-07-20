simulate_season <- function() {
  scores_predicted <- scores
  scores_predicted$H_prob <- NA
  scores_predicted$A_prob <- NA
  scores_predicted$Draw_prob <- NA
  scores_predicted$random_num <- NA
  
  for (i in 1:nrow(scores_predicted)) {
    if(is.na(scores_predicted$H_score[i])) {
      match_prob <- match_probababilities(scores_predicted$Home[i], scores_predicted$Away[i])
      scores_predicted$H_prob[i] <- match_prob$a_win[1]
      scores_predicted$A_prob[i] <- match_prob$b_win[1]
      scores_predicted$Draw_prob[i] <- match_prob$draw[1]
      
      scores_predicted$random_num[i] <- runif(1)
      if (scores_predicted$random_num[i] < scores_predicted$H_prob[i]) {
        scores_predicted$H_score[i] <- 1
        scores_predicted$A_score[i] <- 0
      } else if (scores_predicted$random_num[i] < scores_predicted$H_prob[i] + scores_predicted$A_prob[i]) {
        scores_predicted$H_score[i] <- 0
        scores_predicted$A_score[i] <- 1
      } else {
        scores_predicted$H_score[i] <- 1
        scores_predicted$A_score[i] <- 1
      }
    }
  }
  
  
  #winners and losers
  for (i in 1:nrow(scores_predicted)) {
    if(!is.na(scores_predicted$H_score[i]) & (scores_predicted$H_score[i] == scores_predicted$A_score[i])){
      scores_predicted$Winner[i] <- "Draw"
      scores_predicted$Loser[i] <- "Draw"
      scores_predicted$Draw[i] <- T
    } else if (!is.na(scores_predicted$H_score[i]) & (scores_predicted$H_score[i] != scores_predicted$A_score[i])){
      scores_predicted$Draw[i] <- F
      if (scores_predicted$H_score[i] > scores_predicted$A_score[i]){
        scores_predicted$Winner[i] <- scores_predicted$Home[i]
        scores_predicted$Loser[i] <- scores_predicted$Away[i]
      } else {
        scores_predicted$Winner[i] <- scores_predicted$Away[i]
        scores_predicted$Loser[i] <- scores_predicted$Home[i]
      }
    }
  }
  
  create_table_simulated <- function() {
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
      temp_team <- subset_by_team(team, df = scores_predicted)
      temp_team_played <- temp_team[!is.na(temp_team$H_score),]
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
  
  current_table <- create_table_simulated()
  
  return(current_table)
  #formattable(current_table, row.names = F,
  #            align = c("l", "c", "c", "c", "c", "c", "c", "c", "c", "c"),
  #            list(`Pts`= color_bar("lightblue"),
  #                 `GD` = color_tile("pink", "lightgreen"),
  #                 `GF` = color_bar("lightgreen"),
  #                 `GA` = color_bar("pink"),
  #                 `W` = color_bar("lightgreen"),
  #                 `L` = color_bar("pink"),
  #                 `D` = color_bar("khaki"))
  #)
  
  
  
}

