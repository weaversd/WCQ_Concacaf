#import each matchweek table
matchweek_tables <- list()
matchweek_files <- list.files(path = "current_table_output/", pattern = ".*.csv")

#save each to a different index in the list
for (i in 1:length(matchweek_files)) {
  matchweek_tables[[i]]<- read.csv(paste0("current_table_output/", matchweek_files[i]))
  matchday_str <- as.numeric(str_extract(matchweek_files[i], "\\d+")) ##multiple digits
  matchweek_tables[[i]]$matchday <- matchday_str
}

#combine into one df
weekly_table <- bind_rows(matchweek_tables)


#create a color dictionary

color_list <- c("#ff0000", "#000000", "#ffcc00", "#0073cf", "#009b3a",
                "#006847", "#a7a5a6", "#3C3B6E")
colors <- data.frame(row.names = teams, color = color_list)
teams_v <- unlist(teams)

#to look at the colors for testing:
# color_test_df <- data.frame(teams = teams_v,
#                             number = 1:8,
#                             color = color_list)
# 
# test <- ggplot(data = color_test_df) +
#   geom_point(aes(x = teams, y = number), size = 20, color = color_test_df$color) +
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# test




#add colors to the table
weekly_table$color <- NA
for (i in 1:nrow(weekly_table)) {
  weekly_table$color[i] <- colors[weekly_table$Team[i],]
}

#get current order of teams
current_team_order <- current_table$Team
current_color_order <- rep(NA, 8)
for (i in 1:8) {
  current_color_order[i] <- colors[current_team_order[i],]
}

#keep the order consistent
weekly_table$color <- factor(weekly_table$color, levels = current_color_order)

#make the position plot
position_plot <- ggplot(data = weekly_table, aes(x = matchday, y = Pos)) +
  annotate(geom = "rect", xmin = 1, xmax = max(weekly_table$matchday),
           ymin = 3.5, ymax = 1, fill = "green", alpha = 0.15) +
  annotate(geom = "rect", xmin = 1, xmax = max(weekly_table$matchday),
           ymin = 4.5, ymax = 3.5, fill = "yellow", alpha = 0.15) +
  geom_line(aes(color = color), size = 1.5) +
  scale_y_reverse(breaks = 1:8) +
  scale_color_identity(guide = "legend",
                       labels = current_team_order) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "right", 
        panel.grid.major.x = element_line(color = "grey", linetype = "dashed"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype = "dashed"),
        panel.grid.minor.y = element_blank(),
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.key.height = unit(2.82, "cm")) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
  labs(x = "game", y = "Position", title = "2021-2022 WCQ by game") +
  geom_point(aes(fill = color), size = 3, shape = 21, stroke = 0.5) +
  scale_fill_identity()

#save as png
png("weekly_analysis/position_by_game.png", width = 1200, height = 700)
show(position_plot)
dev.off()


#keep the order consistent alphebetically for the remaining plots
#weekly_table$color <- factor(weekly_table$color, levels = color_list)

top3_plot <- ggplot(data = weekly_table, aes(x = matchday, y = top.3....)) +
  geom_line(aes(color = color), size = 1.5) +
  scale_y_continuous(breaks = seq(0,100,10), limits = c(0,100))+
  scale_color_identity(guide = "legend",
                       labels = current_team_order) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "right", 
        panel.grid = element_blank(),
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
  labs(x = "Game", y = "Chance to finish top 3 (%)", title = "2021-2022 WCQ top 3 (%)") +
  geom_point(aes(fill = color), size = 3, shape = 21, stroke = 0.5) +
  scale_fill_identity()

png("weekly_analysis/top3_chance_by_game.png", width = 1200, height = 700)
show(top3_plot)
dev.off()


#make a points plot
points_plot <- ggplot(data = weekly_table, aes(x = matchday, y = Pts)) +
  geom_line(aes(color = color), size = 1.5) +
  scale_color_identity(guide = "legend",
                       labels = current_team_order) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "right", 
        panel.grid = element_blank(),
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
  scale_y_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
  labs(x = "Game", y = "Points", title = "2021-2022 WCQ points by week") +
  geom_point(aes(fill = color), size = 3, shape = 21, stroke = 0.5) +
  scale_fill_identity()

png("weekly_analysis/points_by_game.png", width = 1200, height = 700)
show(points_plot)
dev.off()

#make a GD plot
GD_plot <- ggplot(data = weekly_table, aes(x = matchday, y = GD)) +
  geom_line(aes(color = color), size = 1.5) +
  scale_color_identity(guide = "legend",
                       labels = current_team_order) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "right", 
        panel.grid = element_blank(),
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
  scale_y_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
  labs(x = "Game", y = "Goal Differential", title = "2021-2022 WCQ goal differential by week") +
  geom_point(aes(fill = color), size = 3, shape = 21, stroke = 0.5) +
  scale_fill_identity()

png("weekly_analysis/goal_differential_by_game.png", width = 1200, height = 700)
show(GD_plot)
dev.off()

#make a GF plot
GF_plot <- ggplot(data = weekly_table, aes(x = matchday, y = GF)) +
  geom_line(aes(color = color), size = 1.5) +
  scale_color_identity(guide = "legend",
                       labels = current_team_order) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "right", 
        panel.grid = element_blank(),
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
  scale_y_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
  labs(x = "Game", y = "Goals scored", title = "2021-2022 WCQ goals scored by week") +
  geom_point(aes(fill = color), size = 3, shape = 21, stroke = 0.5) +
  scale_fill_identity()

png("weekly_analysis/goals_scored_by_game.png", width = 1200, height = 700)
show(GF_plot)
dev.off()

#make a GA plot
GA_plot <- ggplot(data = weekly_table, aes(x = matchday, y = GA)) +
  geom_line(aes(color = color), size = 1.5) +
  scale_color_identity(guide = "legend",
                       labels = current_team_order) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "right", 
        panel.grid = element_blank(),
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
  scale_y_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
  labs(x = "Game", y = "Goals against", title = "2021-2022 WCQ goals against by week") +
  geom_point(aes(fill = color), size = 3, shape = 21, stroke = 0.5) +
  scale_fill_identity()

png("weekly_analysis/goals_against_by_game.png", width = 1200, height = 700)
show(GA_plot)
dev.off()


fourth_plot <- ggplot(data = weekly_table, aes(x = matchday, y = fourth....)) +
  geom_line(aes(color = color), size = 1.5) +
  scale_y_continuous(breaks = seq(0,100,10), limits = c(0,100))+
  scale_color_identity(guide = "legend",
                       labels = current_team_order) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "right", 
        panel.grid = element_blank(),
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
  labs(x = "Game", y = "Chance to finish fourth (%)", title = "2021-2022 WCQ fourth place (%)") +
  geom_point(aes(fill = color), size = 3, shape = 21, stroke = 0.5) +
  scale_fill_identity()

png("weekly_analysis/fourth_chance_by_game.png", width = 1200, height = 700)
show(fourth_plot)
dev.off()
