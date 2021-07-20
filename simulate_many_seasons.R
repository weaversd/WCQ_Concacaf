simulate_n_seasons <- function(iterations = 10) {
  teams_results <- list()
  for (j in 1:8){
    teams_results[[j]] <- data.frame()
  }
  
  for (count in 1:iterations) {
    season_results <- simulate_season()
    for (j in 1:8){
      teams_results[[j]] <- rbind(teams_results[[j]], season_results[season_results$Team == teams[j],])
    }
    
  }
  
  summary_dataframe <- data.frame(matrix(ncol = 12, nrow = 8))
  names <- c("Team", "GP", "W", "L", "D", "Pts", "GF", "GA", "GD", "Pos", "top 3 (%)", "fourth (%)")
  colnames(summary_dataframe) <- names
  for(j in 1:8) {
    team_df <- teams_results[[j]]
    summary_dataframe$Team[j] <- team_df$Team[1]
    summary_dataframe$GP[j] <- mean(team_df$GP)
    summary_dataframe$W[j] <- mean(team_df$W)
    summary_dataframe$L[j] <- mean(team_df$L)
    summary_dataframe$D[j] <- mean(team_df$D)
    summary_dataframe$Pts[j] <- mean(team_df$Pts)
    summary_dataframe$GF[j] <- mean(team_df$GF)
    summary_dataframe$GA[j] <- mean(team_df$GA)
    summary_dataframe$GD[j] <- mean(team_df$GD)
    summary_dataframe$Pos[j] <- mean(team_df$Pos)
    top_3_count <- sum(team_df$Pos <= 3)
    top_3_pct <- top_3_count / iterations
    top_4_count <- sum(team_df$Pos == 4)
    top_4_pct <- top_4_count / iterations
    summary_dataframe$`top 3 (%)`[j] <- top_3_pct *100
    summary_dataframe$`fourth (%)`[j] <- top_4_pct *100

  }
  
  ordered_summary_df <- summary_dataframe[order(-summary_dataframe$Pts,
                                                -summary_dataframe$GD,
                                                -summary_dataframe$GF),]
  
  
  formattable(ordered_summary_df, row.names = F,
              align = c("l", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c"),
              list(`Pts`= color_bar("lightblue"),
                   `GD` = color_tile("pink", "lightgreen"),
                   `GF` = color_bar("lightgreen"),
                   `GA` = color_bar("pink"),
                   `W` = color_bar("lightgreen"),
                   `L` = color_bar("pink"),
                   `D` = color_bar("khaki"),
                   `top 3 (%)` = color_bar("lightgreen")))
  
  
}

simulate_n_seasons(100)