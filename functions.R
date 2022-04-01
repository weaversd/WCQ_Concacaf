team_summary <- function(team){
  team_table <- subset_by_team(team)
  team_played <- team_table[!is.na(team_table$H_score),]
  games_played <- current_table$GP[current_table$Team == team]
  draws <- current_table$D[current_table$Team == team]
  wins <- current_table$W[current_table$Team == team]
  losses <- current_table$L[current_table$Team == team]
  points <- current_table$Pts[current_table$Team == team]
  
  current_position <- which(current_table$Team == team)
  
  
  print(paste0("Team: ", team), quote = F)
  print(paste0("Current Position: ", current_position), quote = F)
  print(paste0("Games Played: ", games_played), quote = F)
  print(paste0("Wins: ", wins), quote = F)
  print(paste0("Losses: ", losses), quote = F)
  print(paste0("Draws: ", draws), quote = F)
  print(paste0("Points: ", points), quote = F)
  print(paste0("Games Left: ", games_n-games_played), quote = F)
  print(paste0("Max possible points: ", (((games_n-games_played)*3)+points)), quote = F)
  
}



#magic number table
magic <- subset(current_table, select = c(Pos, Team, GP, Pts))
magic$GR <- games_n - magic$GP
magic <- magic[,c(1,2,3,5,4)]
teams_order <- magic$Team
for (i in 1:length(teams_order)) {
  magic[[teams_order[i]]] <- NA
}
magic_pct <- magic
magic_ratio <- magic
for (i in 1:length(teams_order)) {
  team_a <- teams_order[i]
  for (j in 1:length(teams_order)){
    team_b <- teams_order[j]
    
    if (team_a == team_b){
      magic_number <- NA
      team_a_percent <- NA
      team_a_character <- "-X-"
    }else {
      team_a_pts <- magic$Pts[i]
      team_b_pts <- magic$Pts[j]
      team_a_GR <- magic$GR[i]
      team_b_GR <- magic$GR[j]
      team_a_max <- team_a_pts + (team_a_GR * 3)
      team_b_max <- team_b_pts + (team_b_GR * 3)
      magic_number <- (team_b_max + 1) - team_a_pts
      team_a_percent <- round((magic_number / (team_a_GR*3)) * 100)
      if (team_a_percent <= 0) {
        team_a_character <- "CLINCHED"
      } else {
        team_a_character <- paste0(magic_number, "/", team_a_GR*3)
      }
      

    }
    magic[[team_b]][i] <- magic_number
    
    magic_pct[[team_b]][i] <- team_a_percent
    
    magic_ratio[[team_b]][i] <- team_a_character
    
    
    if (j < i) {
      #magic[[team_b]][i] <- NA
      #magic_pct[[team_b]][i] <- NA
    }
    
  }

}

#magic number printout
magic_num_print <- function(){
  fourth_place_team <- teams_order[4]
  third_place_team <- teams_order[3]
  for (i in 1:3) {
    if (magic_ratio[[fourth_place_team]][i] == "CLINCHED") {
      print(paste0(teams_order[i], " has clinched at least a tie for third place"))
    } else {
      print(paste0(teams_order[i], " needs to take ", magic_ratio[[fourth_place_team]][i], " points to guarantee at least a tie for third place"))
    }
  }
  for (i in 4:8) {
    print(paste0(teams_order[i], " needs to take ", magic_ratio[[third_place_team]][i], " points to guarantee at least a tie for third place"))
  }
}


#creating plot
magic_plot <- magic_ratio
magic_plot <- subset(magic_plot, select = c(-Pos, -GP, -GR, -Pts))
melted_magic_plot <- data.frame(Team_A = rep(NA, 64), Team_B = NA, Value = NA)

for (i in 1:8) {
  for (j in 1:8) {
    melted_magic_plot$Team_A[j + ((i-1)*8)] <- magic_plot[['Team']][i]
    melted_magic_plot$Team_B[j + ((i-1)*8)] <- magic_plot[['Team']][j]
    melted_magic_plot$Value[j + ((i-1)*8)] <- magic_plot[i,j+1]
  }
}

melted_magic_plot$value_num <- rep(0, 64)
for (i in 1:64) {
  if (melted_magic_plot$Value[i] != "-X-" & melted_magic_plot$Value[i] != "CLINCHED"){
    melted_magic_plot$value_num[i] <- eval(parse(text = melted_magic_plot$Value[i]))
  }
}
melted_magic_plot$X <- rep(1:8, 8)
melted_magic_plot$Y <- rep(8:1, each = 8)

diaganol_df <- data.frame(X = 1:8, Y = 8:1)
highlight_df1 <- data.frame(X = c(1,1,1,1,1,1,1,2),
                            Y = 1:8)
highlight_df2 <- data.frame(X = c(2,2,2,2,2,2,3,3),
                            Y = 1:8)
highlight_df3 <- data.frame(X = c(3,3,3,3,3,4,4,4),
                           Y = 1:8)
highlight_df4 <- data.frame(X = c(4,4,4,4,5,5,5,5),
                           Y = 1:8)
highlight_df1$Position <- "1st"
highlight_df2$Position <- "2nd"
highlight_df3$Position <- "3rd"
highlight_df4$Position <- "4th"

highlight_df <- rbind(highlight_df1, highlight_df2,
                      highlight_df3, highlight_df4)


magic_plot <- ggplot(melted_magic_plot) +
  geom_point(data = highlight_df, aes(X,Y, fill = Position),
             alpha = 0.2, shape = 21, size = 30, stroke = 0, color = "white") +
  geom_point(aes(x = X, y = Y, color = value_num), size = 21, shape = 0, stroke = 2.7) +
  theme_bw(base_size = 20) +
  theme(panel.grid = element_blank(),
        #axis.text.x = element_text(angle = 70, hjust = 0), 
        legend.position = "right",
        axis.text.x = element_blank(),
        axis.ticks = element_blank()) +
  labs(x = element_blank(), y = element_blank(), fill = "At least position: ") +
  scale_x_continuous(breaks = 1:8,
                     labels = teams_order,
                     limits = c(0.8, 8.2),
                     position = "top") +
  scale_y_continuous(breaks = 1:8,
                     labels = rev(teams_order),
                     limits = c(0.8, 8.2)) +
  scale_color_gradient2(low = "green4", high = "red", mid = "yellowgreen", midpoint = 1,
                        guide = "none") +
  geom_point(data = diaganol_df, aes(X,Y), size = 21, shape = 0, stroke = 3) +
  geom_label(aes(X,Y, label = Value)) +
  geom_hline(yintercept = 5.5, color = "blue", size = 2, linetype = 3) +
  #geom_vline(xintercept = 3.5, color = "blue", size = 2, linetype = 3) +
  scale_fill_manual(values = c("green", "turquoise", "blue", "magenta"))


print_final <- function(){
  print(paste0("Predictions based on ", sim_seasons_n, " simulated seasons."), quote = F)
  print(paste0("Predictions are based on margin of victory against an average team from The Power Rank, and using a method based on fivethirtyeight's soccer predictions."), quote = F)
}

