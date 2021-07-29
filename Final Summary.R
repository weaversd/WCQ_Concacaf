#Final table summary
games_played <- sum(!is.na(scores$H_score))
matchdays_raw <- games_played/4
matchdays <- round(matchdays_raw)
if (matchdays_raw%%1 != 0) {
  print("WARNING: Incomplete matchday")
}

matchday_result_tables <- list()

if (matchdays == 0) {
  current_table$Prev <- NA
  current_table$Change <- 0
  positions <- data.frame(matrix(ncol = 8, nrow = 14))
  colnames(positions) <- teams
  
} else {
  for (i in 1:matchdays) {
    matchday_result_tables[[i]] <- scores[1:(4*i),]
  }
  
  
  matchday_result_tables_2 <- list()
  for (i in 1:matchdays) {
    matchday_result_tables_2[[i]] <- create_table(matchday_result_tables[[i]])
  }
  
  positions <- data.frame(matrix(ncol = 8, nrow = 14))
  colnames(positions) <- teams
  
  for (i in 1:matchdays) {
    for (j in 1:8) {
      team_name <- teams[[j]]
      matchday_table <- matchday_result_tables_2[[i]]
      positions[[team_name]][i] <- matchday_table[matchday_table$Team == team_name,][["Pos"]]
    }
  }
  
  current_table$Prev <- NA
  previous_MD <- matchdays -1
  for (j in 1:8) {
    current_team <- current_table$Team[j]
    current_table$Prev[j] <- positions[[current_team]][previous_MD]
    
  }
  current_table$Change <- current_table$Prev - current_table$Pos
  
}

current_table$`top 3 (%)` <- NA
current_table$`fourth (%)` <- NA
for (j in 1:8) {
  current_team <- current_table$Team[j]
  current_table$`top 3 (%)`[j] <- simulated_df[simulated_df$Team == current_team,][['top 3 (%)']]
  current_table$`fourth (%)`[j] <- simulated_df[simulated_df$Team == current_team,][['fourth (%)']]
}


inv_positions <- 9 - positions
sparklines <- rep(NA, 8)
for (j in 1:8) {
  current_team <- current_table$Team[j]
  sparklines[j] <- as.character(htmltools::as.tags(sparkline(inv_positions[[current_team]], type = "line",
                                                             chartRangeMin = 0.8, chartRangeMax = 8,
                                                             fillColor = FALSE,
                                                             minSpotColor = "",
                                                             maxSpotColor = "",
                                                             spotColor = "green",
                                                             lineWidth = 2,
                                                             normalRangeMin = 5.7,
                                                             normalRangeMax = 8.3,
                                                             disableInteraction = T,
                                                             disableTooltips = T,
                                                             disableHighlight = T,
                                                             normalRangeColor = "lightgreen")))
}

current_table$`Pos Trend` <- sparklines

current_table <- current_table[,c(1,10,11,12,15,2,3,4,5,6,7,8,9,13,14)]



out = as.htmlwidget(formattable(current_table, row.names = F,
                                align = c("l", "c", "r", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c"),
                                list(`Pts`= color_bar("lightblue"),
                                     `GD` = color_tile("pink", "lightgreen"),
                                     `GF` = color_bar("lightgreen"),
                                     `GA` = color_bar("pink"),
                                     `W` = color_bar("lightgreen"),
                                     `L` = color_bar("pink"),
                                     `D` = color_bar("khaki"),
                                     `top 3 (%)` = color_bar("lightgreen"),
                                     `Prev` = FALSE,
                                     `Change`= formatter("span",
                                                         style = ~ style(color = ifelse(`Prev` == `Pos`, "black", ifelse(`Prev` > `Pos`, "green", "red"))),
                                                         ~ icontext(sapply(`Change`, function(x) if (x < 0) "arrow-down" else if (x > 0) "arrow-up" else ""), `Change`)))))
out$dependencies = c(out$dependencies, htmlwidgets:::widget_dependencies("sparkline", "sparkline"))
out

