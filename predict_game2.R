match_probababilities_2 <- function(t_a, t_b) {

  t_a_VM <- victory_margins[victory_margins$Team == t_a,]$VM
  t_b_VM <- victory_margins[victory_margins$Team == t_b,]$VM
  
  t_a_VMten <- t_a_VM +10
  t_b_VMten <- t_b_VM +10
  
  a_pois_raw <- rep(0,7)
  
  for (i in 7:13) {
    a_pois_raw[i-6] <- ppois(i, lambda = t_a_VMten)
  }
  #print(a_pois_raw)
  a_pois_hist <- rep(0,8)
  a_pois_hist[1] <- a_pois_raw[1]
  for (i in 2:7) {
    a_pois_hist[i] <- a_pois_raw[i]-a_pois_raw[i-1]
  }
  a_pois_hist[8] <- 1-a_pois_raw[7]
  #print(a_pois_hist)
  
  b_pois_raw <- rep(0,7)
  for (i in 7:13) {
    b_pois_raw[i-6] <- ppois(i, lambda = t_b_VMten)
  }
  #print(b_pois_raw)
  b_pois_hist <- rep(0,8)
  b_pois_hist[1] <- b_pois_raw[1]
  for (i in 2:7) {
    b_pois_hist[i] <- b_pois_raw[i]-b_pois_raw[i-1]
  }
  b_pois_hist[8] <- 1-b_pois_raw[7]
  #print(b_pois_hist)
  
  prob_df <- data.frame(x1 = rep(NA, 8),
                        x2 = rep(NA, 8),
                        x3 = rep(NA, 8),
                        x4 = rep(NA, 8),
                        x5 = rep(NA, 8),
                        x6 = rep(NA, 8),
                        x7 = rep(NA, 8),
                        x8 = rep(NA, 8))
  
  for (i in 1:8) {
    for (j in 1:8) {
      prob_df[i,j] <- a_pois_hist[i] * b_pois_hist[j]
    }
  }
  
  
  draw_sum <- 0
  a_win_sum <- 0
  b_win_sum <- 0
  
  for (i in 1:8) {
    for (j in 1:8) {
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
 # print(prediction_df)
  
  non_draw_pct <- a_win_sum + b_win_sum
  half_draw_pct <- draw_sum /2

  a_win_sum <- a_win_sum - (0.05 - (((a_win_sum/non_draw_pct)-0.5)*0.05))
  b_win_sum <- b_win_sum - (0.05 - (((b_win_sum/non_draw_pct)-0.5)*0.05))
  
  draw_sum <- draw_sum + 0.1
  
  prediction_df <- data.frame(a_win = a_win_sum,
                              b_win = b_win_sum,
                              draw = draw_sum)
  

  #print(prediction_df)
  #print(prob_df)
  return(prediction_df)
  
}
