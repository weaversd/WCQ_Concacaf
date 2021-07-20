match_probababilities <- function(t_a, t_b) {
  t_a_offense <- as.numeric(SPI_teams[SPI_teams$TEAM == t_a,]$offense)
  t_a_defense <- as.numeric(SPI_teams[SPI_teams$TEAM == t_a,]$defense)
  
  t_b_offense <- as.numeric(SPI_teams[SPI_teams$TEAM == t_b,]$offense)
  t_b_defense <- as.numeric(SPI_teams[SPI_teams$TEAM == t_b,]$defense)
  
  t_a_expected <- mean(c(t_a_offense, t_b_defense))
  t_b_expected <- mean(c(t_b_offense, t_a_defense))
  
  a_pois_raw <- rep(0,5)
  
  for (i in 0:4) {
    a_pois_raw[i+1] <- ppois(i, lambda = t_a_expected)
  }
  #print(a_pois_raw)
  a_pois_hist <- rep(0,6)
  a_pois_hist[1] <- a_pois_raw[1]
  for (i in 2:5) {
    a_pois_hist[i] <- a_pois_raw[i]-a_pois_raw[i-1]
  }
  a_pois_hist[6] <- 1-a_pois_raw[5]
  #print(a_pois_hist)
  
  b_pois_raw <- rep(0,5)
  for (i in 0:4) {
    b_pois_raw[i+1] <- ppois(i, lambda = t_b_expected)
  }
  #print(b_pois_raw)
  b_pois_hist <- rep(0,6)
  b_pois_hist[1] <- b_pois_raw[1]
  for (i in 2:5) {
    b_pois_hist[i] <- b_pois_raw[i]-b_pois_raw[i-1]
  }
  b_pois_hist[6] <- 1-b_pois_raw[5]
  #print(b_pois_hist)
  
  prob_df <- data.frame(x1 = rep(NA, 6),
                        x2 = rep(NA, 6),
                        x3 = rep(NA, 6),
                        x4 = rep(NA, 6),
                        x5 = rep(NA, 6),
                        x6 = rep(NA, 6))

  for (i in 1:6) {
    for (j in 1:6) {
      prob_df[i,j] <- a_pois_hist[i] * b_pois_hist[j]
    }
  }
  
  
  draw_sum <- 0
  a_win_sum <- 0
  b_win_sum <- 0
  
  for (i in 1:6) {
    for (j in 1:6) {
      if (i == j) {
        draw_sum <- draw_sum + prob_df[i,j]
      } else if (i > j) {
        a_win_sum <- a_win_sum + prob_df[i,j]
      } else {
        b_win_sum <- b_win_sum + prob_df[i,j]
      }
    }
  }
  
prediction_df <- data.frame(a_win = a_win_sum,
                            b_win = b_win_sum,
                            draw = draw_sum)

#print(prob_df)
return(prediction_df)

}
